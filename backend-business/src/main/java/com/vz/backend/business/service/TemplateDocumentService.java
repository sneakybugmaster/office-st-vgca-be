package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.business.domain.DocumentOutAttachment;
import com.vz.backend.business.domain.TaskAttachment;
import com.vz.backend.business.domain.TemplateDocument;
import com.vz.backend.business.domain.WordEditorProcess;
import com.vz.backend.business.domain.documentInternal.DocInternalAttach;
import com.vz.backend.business.repository.IDocumentOutAttachmentRepository;
import com.vz.backend.business.repository.ITaskAttachmentRepository;
import com.vz.backend.business.repository.ITemplateDocumentRepository;
import com.vz.backend.business.repository.docInternal.IDocInternalAttachRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.AttachmentTypeEnum;
import com.vz.backend.core.config.CalendarStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.FileService;
import com.vz.backend.core.service.FilesStorageService;

@Service
public class TemplateDocumentService extends BaseService<TemplateDocument> {
	
	@Autowired
	FilesStorageService storageService;
	
	@Autowired
	FileService fService;
	
	@Autowired
	ITemplateDocumentRepository templateRepository;
	
	@Autowired
	IDocumentOutAttachmentRepository docOutAttRepository;
	
	@Autowired
	IDocInternalAttachRepository docInternalAttRepository;
	
	@Autowired
	ITaskAttachmentRepository taskAttRepository;
	
	@Autowired
	WordEditorService weService;

	@Override
	public IRepository<TemplateDocument> getRepository() {
		return templateRepository;
	}
	
	public List<TemplateDocument> add(MultipartFile[] files, DocumentTypeEnum type, String tName) {
		if (ArrayUtils.isEmpty(files))
			return Collections.emptyList();
		List<TemplateDocument> aList = new ArrayList<>();
		for (MultipartFile f : files) {
			if (!fService.isDocFile(f))
				throw new RestExceptionHandler(Message.TEMPLATE_FILE_INVALID);
			TemplateDocument a = new TemplateDocument(f, type, tName);
			a.setName(storageService.save(f));
			aList.add(a);
		}
		return templateRepository.saveAll(aList);
	}

	public Page<TemplateDocument>  all(CalendarStatusEnum status, DocumentTypeEnum type, String name, Pageable page) {
		return templateRepository.all(status, type, name, BussinessCommon.getClientId(), page);
	}
	
	public TemplateDocument update(Long id, CalendarStatusEnum status) {
		TemplateDocument t = valid(id, Message.TEMPLATE_NOT_FOUND);
		t.setApproveId(BussinessCommon.getUserId());
		t.setStatus(status);
		return templateRepository.save(t);
	}
	
	public List<TemplateDocument>  list() {
		return templateRepository.findByClientIdAndStatusAndActiveTrue(BussinessCommon.getClientId(), CalendarStatusEnum.APPROVE);
	}

	@SuppressWarnings("unchecked")
	public <T> T use(Long id, DocumentTypeEnum type, String nName) {
		TemplateDocument template = valid(id, Message.TEMPLATE_NOT_FOUND);
		nName = FilesStorageService.parse(nName);
		storageService.copyFile(template.getName(), nName);
		switch (type) {
		case VAN_BAN_DI:
			DocumentOutAttachment a = new DocumentOutAttachment(nName, template.getType(), AttachmentTypeEnum.DRAFT,
					template.getSize());
			a = docOutAttRepository.save(a);
			return (T) a;
		case VAN_BAN_NOI_BO:
			DocInternalAttach a1 = new DocInternalAttach(nName, template.getType(), template.getSize(),
					AttachmentTypeEnum.VAN_BAN);
			a1 = docInternalAttRepository.save(a1);
			return (T) a1;
		case GIAO_VIEC:
			TaskAttachment a2 = new TaskAttachment(nName, template.getType(), template.getSize(), 1L);
			a2 = taskAttRepository.save(a2);
			return (T) a2;
		case VAN_BAN_SOAN_THAO:
			TaskAttachment a3 = new TaskAttachment(nName, template.getType(), template.getSize(), 3L);
			a3 = taskAttRepository.save(a3);
			return (T) a3;
		default:
			return null;
		}
	}

	@SuppressWarnings("unchecked")
	public <T> Page<T> draft(DocumentTypeEnum type, Pageable pageable) {
		Long userId = BussinessCommon.getUserId();
		Long clientId = BussinessCommon.getClientId();
		switch (type) {
		case VAN_BAN_DI:
			return (Page<T>) docOutAttRepository.findByClientIdAndDocIdAndCreateByAndActive(clientId, null, userId,
					true, pageable);
		case VAN_BAN_NOI_BO:
			return (Page<T>)  docInternalAttRepository.findByClientIdAndDocIdAndCreateByAndAttachTypeAndActive(clientId,
					null, userId, AttachmentTypeEnum.VAN_BAN, true, pageable);
		case GIAO_VIEC:
			return (Page<T>) taskAttRepository.findByClientIdAndObjectIdAndCreateByAndTypeObjAndActive(clientId, 0,
					userId, 1L, true, pageable);
		case VAN_BAN_SOAN_THAO:
			return (Page<T>) taskAttRepository.findByClientIdAndObjectIdAndCreateByAndTypeObjAndActive(clientId, 0,
					userId, 3L, true, pageable);
		default:
			return new PageImpl<>(new ArrayList<>(), pageable, 0L);
		}
	}

	@SuppressWarnings("unchecked")
	public <T> T update(Long id, DocumentTypeEnum type, MultipartFile file) {
		Long clientId = BussinessCommon.getClientId();
		String nName = "";
		
		switch (type) {
		case VAN_BAN_DI:
			DocumentOutAttachment a = docOutAttRepository.findByClientIdAndId(clientId, id);
			if (a == null) {
				throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
			}
			nName = storageService.replaces(file, a.getName());
//			a.setName(nName);
//			a = docOutAttRepository.save(a);
			return (T) a;
		case VAN_BAN_NOI_BO:
			DocInternalAttach a1 = docInternalAttRepository.findByClientIdAndId(clientId, id);
			if (a1 == null) {
				throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
			}
			nName = storageService.replaces(file, a1.getName());
//			a1.setName(nName);
//			a1 = docInternalAttRepository.save(a1);
			return (T) a1;
		case GIAO_VIEC:
			return (T) save(id, file);
		case VAN_BAN_SOAN_THAO:
			return (T) save(id, file);
		default:
			return null;
		}
	}

	@SuppressWarnings("unchecked")
	public <T> T update(Long objId, Long aId, DocumentTypeEnum type) {
		Long clientId = BussinessCommon.getClientId();
		switch (type) {
		case VAN_BAN_DI:
			DocumentOutAttachment a = docOutAttRepository.findByClientIdAndId(clientId, aId);
			if (a == null) {
				throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
			}
			a.setDocId(objId);
			a = docOutAttRepository.save(a);
			return (T) a;
		case VAN_BAN_NOI_BO:
			DocInternalAttach a1 = docInternalAttRepository.findByClientIdAndId(clientId, aId);
			if (a1 == null) {
				throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
			}
			a1.setDocId(objId);
			a1 = docInternalAttRepository.save(a1);
			return (T) a1;
		case GIAO_VIEC:
			return (T) save(aId, objId, type);
		case VAN_BAN_SOAN_THAO:
			return (T) save(aId, objId, type);
		default:
			return null;
		}
	}

	private TaskAttachment save(Long id, MultipartFile file) {
		TaskAttachment a2 = taskAttRepository.findByClientIdAndId(BussinessCommon.getClientId(), id);
		if (a2 == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}
		String nName = storageService.replaces(file, a2.getName());
//		a2.setName(nName);
//		taskAttRepository.save(a2);
		return a2;
	}
	
	private void del(Long id) {
		TaskAttachment a2 = taskAttRepository.findByClientIdAndId(BussinessCommon.getClientId(), id);
		if (a2 == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}
		storageService.delete(a2.getName());
		taskAttRepository.delete(a2);
	}

	private TaskAttachment save(Long aId, Long objId, DocumentTypeEnum type) {
		TaskAttachment a2 = taskAttRepository.findByClientIdAndId(BussinessCommon.getClientId(), aId);
		if (a2 == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}
		if(DocumentTypeEnum.VAN_BAN_SOAN_THAO.equals(type)) {
			WordEditorProcess p = weService.getProcess(BussinessCommon.getUserId(), objId);
			a2.setObjectId(p.getId());
		} else {
			a2.setObjectId(objId);
		}
		
		return taskAttRepository.save(a2);
	}

	public Boolean del(Long aId, DocumentTypeEnum type) {
		Long clientId = BussinessCommon.getClientId();
		switch (type) {
		case VAN_BAN_DI:
			DocumentOutAttachment a = docOutAttRepository.findByClientIdAndId(clientId, aId);
			if (a == null) {
				throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
			}
			storageService.delete(a.getName());
			docOutAttRepository.delete(a);
			break;
		case VAN_BAN_NOI_BO:
			DocInternalAttach a1 = docInternalAttRepository.findByClientIdAndId(clientId, aId);
			if (a1 == null) {
				throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
			}
			storageService.delete(a1.getName());
			docInternalAttRepository.delete(a1);
			break;
		case GIAO_VIEC:
			del(aId);
			break;
		case VAN_BAN_SOAN_THAO:
			del(aId);
			break;
		case VAN_BAN_MAU:
			TemplateDocument t = templateRepository.findByClientIdAndId(BussinessCommon.getClientId(), aId);
			if (t == null) {
				throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
			}
			storageService.delete(t.getName());
			templateRepository.delete(t);
			break;
		default:
		}
		return true;
	}
}
