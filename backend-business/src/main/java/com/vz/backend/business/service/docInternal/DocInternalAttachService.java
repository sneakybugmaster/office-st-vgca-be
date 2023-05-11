package com.vz.backend.business.service.docInternal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.business.config.DocInternalTrackingEnum;
import com.vz.backend.business.domain.documentInternal.DocInternalAttach;
import com.vz.backend.business.dto.document.DocInternalAttachDto;
import com.vz.backend.business.repository.docInternal.IDocInternalAttachRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.AttachmentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.FilesStorageService;

@Service
public class DocInternalAttachService extends BaseService<DocInternalAttach> {
	@Autowired
	private IDocInternalAttachRepository docInternalAttachRepo;
	
	@Autowired
	private FilesStorageService storageService;
	
	@Autowired
	private DocInternalService docInternalService;
	
	@Autowired
	private DocInternalTrackingService docInternalTrackingService;
	
	@Autowired
	private DocInternalCommentService docInternalCmtService;
	
	@Override
	public IRepository<DocInternalAttach> getRepository() {
		return docInternalAttachRepo;
	}

	public void add(Long docId, MultipartFile[] files, AttachmentTypeEnum type) {
		List<DocInternalAttach> aList = new ArrayList<>();
		for (MultipartFile f : files) {
			DocInternalAttach a = new DocInternalAttach();
			a.setName(storageService.saveDocInternal(f));
			a.setType(f.getContentType());
			a.setAttachType(type);
			a.setSize(f.getSize());
			a.setDocId(docId);
			aList.add(a);
		}
		docInternalAttachRepo.saveAll(aList);
	}

	public List<DocInternalAttachDto> getListAttachByDocId(Long docId) {
		return docInternalAttachRepo.getListAttachByDocIdAndClientId(docId, BussinessCommon.getClientId());
	}

	public void deactiveByIdIn(List<Long> deleteIds) {
		docInternalAttachRepo.deactiveByIdInAndClientId(deleteIds, BussinessCommon.getClientId());
	}

	public DocInternalAttach getFileByIdOrFileName(Long attachId, String fileName) {
		DocInternalAttach attach = attachId != null
				? docInternalAttachRepo.findByClientIdAndId(BussinessCommon.getClientId(), attachId)
				: docInternalAttachRepo.findFirstByClientIdAndNameAndActiveTrue(fileName,
						BussinessCommon.getClientId());
		if (attach == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}
		
		if (attach.getDocId() != null) {
			// Validate quyền truy cập văn bản của user
			docInternalService.validatePermission(attach.getDocId());
			
			// Add tracking
			docInternalTrackingService.add(attach.getDocId(), DocInternalTrackingEnum.DOWNLOAD_FILE);
		}
		
		return attach;
	}
	
	public DocInternalAttach getAttachById(Long attachId) {
		DocInternalAttach attach = docInternalAttachRepo.findByClientIdAndId(BussinessCommon.getClientId(), attachId);
		return attach;
	}
	
	public DocInternalAttach updateSignFile(DocInternalAttach doa, MultipartFile file) {
		String fileName = doa.getName();
		if (fileName.lastIndexOf(".doc") > 0) {
			String ext = fileName.substring(fileName.lastIndexOf(".doc"), fileName.lastIndexOf("__"));
			fileName = fileName.replace(ext, ".pdf");
			storageService.saveDocInternal(file, fileName);
		} else {
			fileName = doa.getName();
			storageService.saveDocInternal(file, fileName);
		}

		doa.setName(fileName);
		doa.setType(file.getContentType());
		doa.setSize(file.getSize());
		return docInternalAttachRepo.save(doa);
	}

	public DocInternalAttach add(MultipartFile encrypted, Long objId, String encryptName, AttachmentTypeEnum type) {
		boolean isComment = true;
		if (AttachmentTypeEnum.VAN_BAN.equals(type) || AttachmentTypeEnum.PHU_LUC.equals(type)) {
			docInternalService.valid(objId, Message.NOT_FOUND_OBJECT);
			isComment = false;
		}
		return docInternalAttachRepo.save(new DocInternalAttach(encrypted, objId, encryptName, type, isComment));
	}
	
	public List<DocInternalAttach> addListAttachmentComment(MultipartFile[] files, Long docInternalCmtId, boolean skipError) {
		
		if(!skipError) {
			docInternalCmtService.valid(docInternalCmtId, Message.NOT_FOUND_DOC_INTERNAL_COMMENT);
		}
		
		if (ArrayUtils.isEmpty(files)) {
			return Collections.emptyList();
		}
		List<DocInternalAttach> aList = new ArrayList<>();
		for (MultipartFile f : files) {
			DocInternalAttach a = new DocInternalAttach();
			a.setName(storageService.save(f));
			a.setType(f.getContentType());
			a.setSize(f.getSize());
			a.setDocInternalCommentId(docInternalCmtId);;
			a.setAttachType(AttachmentTypeEnum.BINH_LUAN);
			aList.add(a);
		}
		return docInternalAttachRepo.saveAll(aList);
	}
	
}
