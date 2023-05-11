package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.business.domain.AttachmentComment;
import com.vz.backend.business.domain.DocumentComment;
import com.vz.backend.business.repository.IAttachmentCommentRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.FilesStorageService;

@Service
public class AttachmentCommentService extends BaseService<AttachmentComment> {
	@Autowired
	private IAttachmentCommentRepository relateRepository;

	@Autowired
	FilesStorageService storageService;

	@Autowired
	DocumentService docService;

	@Autowired
	DocumentCommentService docCmtService;

	@Override
	public IRepository<AttachmentComment> getRepository() {
		return relateRepository;
	}

	public List<AttachmentComment> addListAttachmentComment(MultipartFile[] files, Long objId) {
		if (ArrayUtils.isEmpty(files) || files == null || files.length == 0) {
			return Collections.emptyList();
		}
		List<AttachmentComment> aList = new ArrayList<>();
		for (MultipartFile f : files) {
			AttachmentComment a = new AttachmentComment();
			a.setName(storageService.save(f));
			a.setType(f.getContentType());
			a.setSize(f.getSize());
			a.setCommentId(objId);
			aList.add(a);
		}
		return relateRepository.saveAll(aList);
	}

	public AttachmentComment updateFile(Long id, MultipartFile file) {
		Optional<AttachmentComment> optional = this.findById(id);
		if (!optional.isPresent()) {
			throw new RestExceptionHandler("Can't find attachment with id: " + id);
		}
		AttachmentComment attachment = optional.get();
		if (!BussinessCommon.getUser().getId().equals(attachment.getCreateBy())) {
			throw new RestExceptionHandler("You are not the owner of this file");
		}
		String oldName = attachment.getName();
		String newName = storageService.replace(file, oldName);
		attachment.setName(newName);
		return relateRepository.save(attachment);
	}

	@Override
	public void deleteById(Long id) {
		Optional<AttachmentComment> optional = this.findById(id);
		if (!optional.isPresent()) {
			throw new RestExceptionHandler("Can't find attachment with id: " + id);
		}
		AttachmentComment attachment = optional.get();
		if (!BussinessCommon.getUser().getId().equals(attachment.getCreateBy())) {
			throw new RestExceptionHandler("You are not the owner of this file");
		}
		delete(attachment);
	}
	
	@Override
	public void delete(AttachmentComment a) {
		if (a == null) return;
		storageService.deleteFile(a.getName());
		relateRepository.delete(a);
	}

	public AttachmentComment validDownloadFile(String name) {
		AttachmentComment a = findFileByName(name);
		DocumentComment docComment = docCmtService.findByIdCmt(a.getCommentId());
		if (!docService.checkPermission(docComment.getDocId())) {
			throw new RestExceptionHandler(Message.ATTACHMENT_FILE_NOT_PERMIT);
		}
		return a;
	}

	public AttachmentComment findFileByName(String name) {
		AttachmentComment files = relateRepository.findByNameAndClientIdAndActive(name, BussinessCommon.getClientId(),
				true);
		if (files == null) {
			throw new RestExceptionHandler(Message.ATTACHMENT_FILE_NOT_FOUND);
		}
		return files;
	}
	
	public AttachmentComment add(MultipartFile file, Long objId, String name) {
		return relateRepository.save(new AttachmentComment(file, objId, name));
	}
	
	/**
	 * Xóa nội dung bình luận và đính kèm
	 * 
	 * @param name
	 */
	public void delByAttachAndComment(String[] names, Long cmtIdSaved) {
		if (ArrayUtils.isEmpty(names) && cmtIdSaved == null)
			return;

		Long clientId = BussinessCommon.getClientId();
		AttachmentComment a;
		if (names != null) {
			for (String i : names) {
				a = relateRepository.findByNameAndClientIdAndActive(i, clientId, true);
				if (a != null) {
					delete(a);
				}
			}
		}

		if (cmtIdSaved == null)
			return;

		DocumentComment d = docCmtService.findByClientIdAndId(clientId, cmtIdSaved);
		if (d != null && Boolean.TRUE.equals(d.getActive())) {
			d.setActive(false);
			docCmtService.save(d);
		}
	}
}
