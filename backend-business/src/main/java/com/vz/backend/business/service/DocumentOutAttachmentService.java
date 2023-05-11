package com.vz.backend.business.service;

import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import com.vz.backend.business.domain.AttachmentVersion;
import com.vz.backend.business.repository.IAttachmentVersionRepository;
import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.DocumentOutAttachment;
import com.vz.backend.business.domain.DocumentOutProcess;
import com.vz.backend.business.dto.hstl.ecm.FolderAttachmentDto;
import com.vz.backend.business.repository.IDocumentOutAttachmentRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.AttachmentTypeEnum;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentOutHandleStatusEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.EncryptionService;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.util.StringUtils;
import com.vz.backend.util.Utils;

import net.lingala.zip4j.ZipFile;
import net.lingala.zip4j.exception.ZipException;
import net.lingala.zip4j.model.ZipParameters;
import net.lingala.zip4j.model.enums.EncryptionMethod;

@Service
public class DocumentOutAttachmentService extends BaseService<DocumentOutAttachment> {

	@Autowired
	IDocumentOutAttachmentRepository docOutAttachRepository;

	@Autowired
	private IAttachmentVersionRepository attachVersionRepository;

	@Autowired
	FilesStorageService storageService;

	@Autowired
	private DocumentOutService docOutService;

	@Autowired
	private DocumentOutProcessService docOutProcessService;

	@Autowired
	private DelegateService delegateService;

	@Autowired
	private DocumentOutCommentService docOutCmtService;
	
	@Autowired
	private EncryptionService encryptService;

	@Autowired
	private AttachmentVersionService attachmentVersionService;
	
	@Override
	public IRepository<DocumentOutAttachment> getRepository() {
		return docOutAttachRepository;
	}

	public List<DocumentOutAttachment> addListAttachment(MultipartFile[] files, String attType, Long objId) {
		if (ArrayUtils.isEmpty(files)) {
			return Collections.emptyList();
		}
		List<DocumentOutAttachment> aList = new ArrayList<>();
		if (attType.equals(AttachmentTypeEnum.DRAFT.getName())) {
			for (MultipartFile f : files) {
				DocumentOutAttachment a = new DocumentOutAttachment();
				a.setName(storageService.save(f));
				a.setType(f.getContentType());
				a.setAttachmentType(AttachmentTypeEnum.DRAFT);
				a.setSize(f.getSize());
				a.setDocId(objId);
				aList.add(a);
				attachmentVersionService.addFirstVersion(f,objId, a.getName());
			}
		} else if (attType.equals(AttachmentTypeEnum.RELATE.getName())) {
			for (MultipartFile f : files) {
				DocumentOutAttachment a = new DocumentOutAttachment();
				a.setName(storageService.save(f));
				a.setType(f.getContentType());
				a.setAttachmentType(AttachmentTypeEnum.RELATE);
				a.setSize(f.getSize());
				a.setDocId(objId);
				aList.add(a);
			}
		} else if (attType.equals(AttachmentTypeEnum.COMMENT.getName())) {
			for (MultipartFile f : files) {
				DocumentOutAttachment a = new DocumentOutAttachment();
				a.setName(storageService.save(f));
				a.setType(f.getContentType());
				a.setAttachmentType(AttachmentTypeEnum.COMMENT);
				a.setSize(f.getSize());
				a.setCmtId(objId);
				aList.add(a);
			}
		} else {
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
		}

		return docOutAttachRepository.saveAll(aList);
	}

	public void deleteAllByDocId(Long docId) {
		try {
			List<String> fileNames = docOutAttachRepository.getFileNameByDocId(docId);
			docOutAttachRepository.deleteByDocId(docId);
			fileNames.forEach(name -> storageService.deleteFile(name));
		} catch (Exception e) {
			e.printStackTrace();
		}

	}

	public DocumentOutAttachment updateFile(Long id, MultipartFile file) {
		Optional<DocumentOutAttachment> docOptional = this.findById(id);
		if (!docOptional.isPresent()) {
			throw new RestExceptionHandler("Can't find attachment with id: " + id);
		}
		DocumentOutAttachment documentOutAttachment = docOptional.get();
		if (!BussinessCommon.getUser().getId().equals(documentOutAttachment.getCreateBy())) {
			throw new RestExceptionHandler("You are not the owner of this file");
		}
		String oldName = documentOutAttachment.getName();
		String newName = storageService.replace(file, oldName);
		documentOutAttachment.setName(newName);
		return docOutAttachRepository.save(documentOutAttachment);
	}

	public DocumentOutAttachment updateFile(DocumentOutAttachment doa, MultipartFile file) {
		if (!BussinessCommon.getUser().getId().equals(doa.getCreateBy())) {
			throw new RestExceptionHandler("You are not the owner of this file");
		}
		String oldName = doa.getName();
		String newName = storageService.replace(file, oldName);
		doa.setName(newName);
		return docOutAttachRepository.save(doa);
	}

	public DocumentOutAttachment updateSignFile(DocumentOutAttachment doa, MultipartFile file) {
		String fileName = "";
		if (doa.getName().toLowerCase().indexOf(".doc") > 0 || doa.getName().toLowerCase().indexOf(".odt") > 0) {
			fileName = file.getOriginalFilename();
			int indexToken = fileName.indexOf("?token");
			if (indexToken > 0) {
				fileName = fileName.substring(0, indexToken);
				storageService.saveToSystem(file, fileName);
			} else {
				storageService.saveToSystem(file, fileName);
			}
		} else {
			fileName = doa.getName();
			storageService.saveToSystem(file, fileName);
		}

		doa.setName(fileName);
		doa.setType(file.getContentType());
		doa.setSize(file.getSize());
		return docOutAttachRepository.save(doa);
	}

	public DocumentOutAttachment editFile(DocumentOutAttachment doa, MultipartFile file, boolean edit) {
		String fileName = FilesStorageService.getFileName(file);
		storageService.saveToSystem(file, fileName);
		doa.setName(fileName);
		doa.setType(file.getContentType());
		doa.setSize(file.getSize());
		if (edit) {
			doa.setIsChanged(true);
		}
		return docOutAttachRepository.save(doa);
	}
	public DocumentOutAttachment editFileUpload(DocumentOutAttachment doa, MultipartFile file, boolean edit) {
		String fileName = doa.getName();
		int indexDot = 0;
		try {
			Integer maxVersion = attachVersionRepository.getMaxVersionByDocId(doa.getDocId(), BussinessCommon.getClientId());
			if (maxVersion == null) {
				maxVersion = 1;
				indexDot =fileName.lastIndexOf(".");
			} else {
				maxVersion += 1;
				indexDot =fileName.lastIndexOf("_v");
				if(indexDot == -1){
					indexDot =fileName.lastIndexOf(".");
				}
			}
			fileName = fileName.substring(0, indexDot) + "_v" + maxVersion + fileName.substring(fileName.lastIndexOf("."), fileName.length());
			AttachmentVersion a = new AttachmentVersion();
			a.setName(fileName);
			a.setOriginName(fileName);
			a.setType(doa.getType());
			a.setSize(doa.getSize());
			a.setVersion(maxVersion);
			a.setDocId(doa.getDocId());
			a.setUserFullName(BussinessCommon.getUser().getFullName());
			attachVersionRepository.save(a);
		}catch (Exception e){
			e.printStackTrace();
		}

		storageService.saveToSystem(file, fileName);
		doa.setName(fileName);
		doa.setType(file.getContentType());
		doa.setSize(file.getSize());
		if (edit) {
			doa.setIsChanged(true);
		}
		return docOutAttachRepository.save(doa);
	}

	@Override
	public void deleteById(Long id) {
		Optional<DocumentOutAttachment> docOptional = this.findById(id);
		if (!docOptional.isPresent()) {
			throw new RestExceptionHandler("Can't find attachment with id: " + id);
		}
		DocumentOutAttachment documentOutAttachment = docOptional.get();
		if (!BussinessCommon.getUser().getId().equals(documentOutAttachment.getCreateBy())) {
			throw new RestExceptionHandler("You are not the owner of this file");
		}
		String oldName = documentOutAttachment.getName();
		storageService.deleteFile(oldName);
		docOutAttachRepository.delete(documentOutAttachment);
	}

	@Override
	public void delete(DocumentOutAttachment doa) {
		if (!BussinessCommon.getUser().getId().equals(doa.getCreateBy())) {
			throw new RestExceptionHandler("You are not the owner of this file");
		}
		String oldName = doa.getName();
		storageService.deleteFile(oldName);
		docOutAttachRepository.delete(doa);
	}

	public void encrypt(DocumentOut documentOut) {
		List<DocumentOutAttachment> attachments = documentOut.getAttachments();

		String password = StringUtils.randomPassword(Constant.DEFAULT_LENGTH_PASSWORD_FILE);
		documentOut.setPassword(password);
		String zipName = "Văn bản [" + documentOut.getId() + "] " + Utils.coalesce(documentOut.getNumberOrSign(), "0");
		zipName = zipName.replaceAll("[\\/:*?\"<>|]", "_");
		zipName = FilesStorageService.parse(zipName + ".zip");
		Path zipPath = storageService.getPath(zipName);
		File zip = zipPath.toFile();
		ZipFile zipFile = new ZipFile(zip, password.toCharArray());
		for (DocumentOutAttachment attachment : attachments) {
			try {
				String name = attachment.getName();
				File file = storageService.getFile(name);
				if (file.exists()) {
					String origin = FilesStorageService.origin(name);
					ZipParameters parameters = new ZipParameters();
					parameters.setEncryptFiles(true);
					parameters.setEncryptionMethod(EncryptionMethod.ZIP_STANDARD);
					parameters.setFileNameInZip(attachment.toFolder() + "/" + origin);
					zipFile.addFile(file, parameters);
					storageService.deleteFile(name);
				}
				attachment.setActive(false);
			} catch (ZipException e) {
				e.printStackTrace();
			}
		}
		DocumentOutAttachment encryptAttachment = new DocumentOutAttachment();
		encryptAttachment.setName(zip.getName());
		encryptAttachment.setSize(zip.length());
		encryptAttachment.setActive(true);
		//		encryptAttachment.setAttachmentType(AttachmentTypeEnum.ENCRYPT);
		encryptAttachment.setAttachmentType(AttachmentTypeEnum.DRAFT);
		encryptAttachment.setDocId(documentOut.getId());
		encryptAttachment.setType("application/x-zip-compressed");
		encryptAttachment = docOutAttachRepository.save(encryptAttachment);
		attachments.add(encryptAttachment);
	}

	public boolean existAttachmentByTypeAndDocId(AttachmentTypeEnum type, long docId) {
		return docOutAttachRepository.existAttachmentByTypeAndDocIdAndClientId(type, docId,
				BussinessCommon.getClientId());
	}

	public boolean existAttachmentByFileNameAndDocId(String fileName, long docId) {
		return docOutAttachRepository.existAttachmentByFileNameAndDocIdAndClientId(fileName, docId,
				BussinessCommon.getClientId());
	}

	public Optional<DocumentOutAttachment> findByName(String fileName) {
		return docOutAttachRepository.findByName(fileName);
	}

	public List<DocumentOutAttachment> getListAttachment(Long docId) {
		return docOutAttachRepository.findAllByDocIdAndType(AttachmentTypeEnum.DRAFT, docId,
				BussinessCommon.getClientId());
	}
	
	public boolean deleteAttachmentsById(Long id, Boolean force) {
		Optional<DocumentOutAttachment> docOptional = docOutAttachRepository.findById(id);
		if (!docOptional.isPresent()) {
			throw new RestExceptionHandler("Can't find attachment with id: " + id);
		}

		DocumentOutAttachment a = docOptional.get();
		Long docId = a.getDocId();

		// Nếu sử dụng template mà không mã hóa => check trạng thái
		// Nếu sử dụng template và có mã hóa file => không check trạng thái
		if (docId != null && !Boolean.TRUE.equals(force)) {
			// Validate quyền update văn bản của user
			if (!docOutService.checkStatusInList(docId, Arrays.asList(DocumentStatusEnum.DU_THAO,
					DocumentStatusEnum.BI_TRA_LAI, DocumentStatusEnum.DANG_XU_LY))) {
				throw new RestExceptionHandler("Trạng thái văn bản không cho phép xóa đính kèm");
			}
			
			Long currUser = BussinessCommon.getUserId();
			// Check process status
			DocumentOutHandleStatusEnum[] enums = new DocumentOutHandleStatusEnum[] {
					DocumentOutHandleStatusEnum.DU_THAO, DocumentOutHandleStatusEnum.CHO_XU_LY,
					DocumentOutHandleStatusEnum.BI_TRA_LAI };
			DocumentOutProcess input = docOutProcessService
					.findFirstByActiveAndDocIdAndHandleStatusInOrderByIdDesc(true, docId, enums);
			if (input == null) {
				throw new RestExceptionHandler(Message.INVALID_PROCESS);
			}
			
			Long fromUserId = input.getUserId();
			// Check ủy quyền
			if (!(currUser.equals(fromUserId)
					|| delegateService.existFromUserByToUser(fromUserId, currUser, new Date()))) {
				throw new RestExceptionHandler("Không có quyền xóa tệp đính kèm...");
			}
		}
		
		if (Boolean.TRUE.equals(a.getEncrypt())) {
			encryptService.delEncrypt(a.getName());
		} else {
			storageService.delete(a.getName());
		}
		docOutAttachRepository.delete(a);
		
		return true;
	}
	
	public DocumentOutAttachment add(MultipartFile file, Long objId, String name, AttachmentTypeEnum type) {
		if(AttachmentTypeEnum.RELATE.equals(type) || AttachmentTypeEnum.DRAFT.equals(type)) {
			docOutService.valid(objId, Message.NOT_FOUND_OBJECT);
		}
		
		if(AttachmentTypeEnum.COMMENT.equals(type)) {
			docOutCmtService.valid(objId, Message.NOT_FOUND_OBJECT);
		}
		
		return docOutAttachRepository.save(new DocumentOutAttachment(file, objId, name, type));
	}
	
	public List<FolderAttachmentDto> getByFolderIds(List<Long> folderIds) {
		return docOutAttachRepository.getByFolderIds(folderIds, BussinessCommon.getClientId());
	}
}
