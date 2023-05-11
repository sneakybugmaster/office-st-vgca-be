package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.business.domain.TaskAttachment;
import com.vz.backend.business.repository.ITaskAttachmentRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.FilesStorageService;

@Service
public class TaskAttachmentService extends BaseService<TaskAttachment> {
	@Autowired
	private ITaskAttachmentRepository repository;

	@Autowired
	FilesStorageService storageService;

	@Autowired
	DocumentService docService;

	@Autowired
	TaskCommentService taskCommentService;
	
	@Autowired
	TaskService taskService;

	@Override
	public IRepository<TaskAttachment> getRepository() {
		return repository;
	}

	public List<TaskAttachment> addListAttachment(MultipartFile[] files, Long objId, Long type) {
		if (ArrayUtils.isEmpty(files)) {
			return Collections.emptyList();
		}
		List<TaskAttachment> aList = new ArrayList<>();
		for (MultipartFile f : files) {
			TaskAttachment a = new TaskAttachment();
			a.setName(storageService.save(f));
			a.setType(f.getContentType());
			a.setSize(f.getSize());
			a.setObjectId(objId);
			a.setTypeObj(type);
			aList.add(a);
		}
		return repository.saveAll(aList);
	}
	
	public TaskAttachment findByCommentId(Long commmentId) {
		TaskAttachment taskComment = repository.findByObjectId(commmentId);
		return taskComment;
	}

	public List<TaskAttachment> findByCommentId(List<Long> commmentId, Long type, Long clientId, boolean active) {
		return repository.getByCmtIds(commmentId, type, clientId, active);
	}

	public List<TaskAttachment> findByObjId(Long objId, Long type, Long clientId, boolean active) {
		return repository.getByObjId(objId, type, clientId, active);
	}

	public TaskAttachment validDownloadFile(String name) {
		TaskAttachment a = findFileByName(name);
		return a;
	}

	public TaskAttachment findFileByName(String name) {
		TaskAttachment files = repository.findByNameAndClientIdAndActive(name, BussinessCommon.getClientId(), true);
		if (files == null) {
			throw new RestExceptionHandler(Message.ATTACHMENT_FILE_NOT_FOUND);
		}
		return files;
	}
	
	public List<TaskAttachment> getAttachsByCmtId(List<TaskAttachment> all, Long cmtId) {
		List<TaskAttachment> attList = new ArrayList<>();
		all.forEach(i -> {
			if (cmtId.equals(i.getObjectId())) {
				attList.add(i);
			}
		});
		return attList;
	}
	
	public TaskAttachment add(MultipartFile file, Long objId, String name, Long objType) {
		if (objType.longValue() == 1) {
			taskService.valid(objId, Message.NOT_FOUND_OBJECT);
		}

		if (objType.longValue() == 2) {
			taskCommentService.valid(objId, Message.NOT_FOUND_OBJECT);
		}

		return repository.save(new TaskAttachment(file, objId, name, objType));
	}
}
