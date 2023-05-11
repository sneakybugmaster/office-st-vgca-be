package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.dom4j.DocumentType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.Documents;
import com.vz.backend.business.domain.ObjectTag;
import com.vz.backend.business.domain.Tag;
import com.vz.backend.business.domain.Task;
import com.vz.backend.business.domain.documentInternal.DocumentInternal;
import com.vz.backend.business.dto.ObjectTagDto;
import com.vz.backend.business.repository.IDocumentOutRepository;
import com.vz.backend.business.repository.IDocumentRepository;
import com.vz.backend.business.repository.IObjectTagRepository;
import com.vz.backend.business.repository.ITaskRepository;
import com.vz.backend.business.repository.docInternal.IDocInternalRepository;
import com.vz.backend.business.service.docInternal.DocInternalService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.util.StringUtils;

@Service
public class ObjectTagService extends BaseService<ObjectTag> {

	@Autowired
	private IObjectTagRepository objTagRepository;

	@Autowired
	private ITaskRepository taskRepository;

	@Autowired
	private IDocumentRepository docInRepository;

	@Autowired
	private IDocumentOutRepository docOutRepository;

	@Autowired
	private IDocInternalRepository docInternalRepository;

	@Override
	public IRepository<ObjectTag> getRepository() {
		return objTagRepository;
	}

	@Autowired
	private TagService tagService;

	@Autowired
	private TaskService taskService;

	@Autowired
	private DocumentService docInService;

	@Autowired
	private DocumentOutService docOutService;

	@Autowired
	private DocInternalService docInternalService;

	private void validObjectExist(Long objId, DocumentTypeEnum type) {
		if (DocumentTypeEnum.GIAO_VIEC.equals(type)) {
			taskService.valid(objId, Message.TASK_NOT_FOUND);
		}

		if (DocumentTypeEnum.VAN_BAN_DEN.equals(type)) {
			docInService.valid(objId, Message.NOT_FOUND_DOC_IN);
		}

		if (DocumentTypeEnum.VAN_BAN_DI.equals(type)) {
			docOutService.valid(objId, Message.NOT_FOUND_DOC_OUT);
		}

		if (DocumentTypeEnum.VAN_BAN_NOI_BO.equals(type)) {
			docInternalService.valid(objId, Message.NOT_FOUND_DOCUMENT_INTERNAL);
		}
	}

	public ObjectTag assign(Long tagId, Long objId, DocumentTypeEnum type) {
		if (tagId == null || objId == null || type == null)
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);

		List<DocumentTypeEnum> checked = new ArrayList<>();
		checked.add(DocumentTypeEnum.VAN_BAN_DEN);
		checked.add(DocumentTypeEnum.VAN_BAN_DI);
		checked.add(DocumentTypeEnum.GIAO_VIEC);
		checked.add(DocumentTypeEnum.VAN_BAN_NOI_BO);

		if (!checked.contains(type))
			throw new RestExceptionHandler(Message.INVALID_OBJECT_TAG_TYPE);

		tagService.valid(tagId, Message.NOT_FOUND_TAG);
		validObjectExist(objId, type);

		ObjectTag exist = objTagRepository.findByClientIdAndTagIdAndObjIdAndType(BussinessCommon.getClientId(), tagId,
				objId, type);
		if (exist != null && Boolean.TRUE.equals(exist.getActive()))
			throw new RestExceptionHandler(Message.OBJECT_TAG_EXIST);

		if (exist != null && Boolean.FALSE.equals(exist.getActive())) {
			exist.setActive(true);
			return objTagRepository.save(exist);
		}

		ObjectTag rs = new ObjectTag(objId, tagId, type, getNameOrPreview(objId, type));
		setDateObjectTag(rs);
		return objTagRepository.save(rs);
	}

	public boolean untag(Long objId, Long tagId, DocumentTypeEnum type) {
		ObjectTag exist = objTagRepository
				.findByClientIdAndObjIdAndTagIdAndTypeAndActiveTrue(BussinessCommon.getClientId(), objId, tagId, type);
		if(exist == null)
			throw new RestExceptionHandler(Message.OBJECT_TAG_NOT_FOUND);
		exist.setActive(false);
		objTagRepository.save(exist);
		return true;
	}

	private String getNameOrPreview(Long objId, DocumentTypeEnum type) {
		String rs = "";

		if (DocumentTypeEnum.GIAO_VIEC.equals(type)) {
			Task task = taskRepository.findByClientIdAndId(BussinessCommon.getClientId(), objId);
			rs = StringUtils.isNullOrEmpty(task.getTaskName()) ? "" : task.getTaskName();
		}

		if (DocumentTypeEnum.VAN_BAN_DEN.equals(type)) {
			Documents docIn = docInRepository.findByClientIdAndId(BussinessCommon.getClientId(), objId);
			rs = StringUtils.isNullOrEmpty(docIn.getPreview()) ? "" : docIn.getPreview();
		}

		if (DocumentTypeEnum.VAN_BAN_DI.equals(type)) {
			DocumentOut docOut = docOutRepository.findByClientIdAndId(BussinessCommon.getClientId(), objId);
			rs = StringUtils.isNullOrEmpty(docOut.getPreview()) ? "" : docOut.getPreview();
		}

		if (DocumentTypeEnum.VAN_BAN_NOI_BO.equals(type)) {
			DocumentInternal docInternal = docInternalRepository.findByClientIdAndId(BussinessCommon.getClientId(),
					objId);
			rs = StringUtils.isNullOrEmpty(docInternal.getPreview()) ? "" : docInternal.getPreview();
		}

		return rs;
	}

	public Page<ObjectTagDto> getListObjectTagByTagId(Long tagId, int page, String keyWord) {

		Page<ObjectTagDto> rs = objTagRepository.findByClientIdAndTagIdAndActiveTrue(BussinessCommon.getClientId(),
				tagId, keyWord.toLowerCase(), BussinessCommon.castToPageable(page));
		for (ObjectTagDto obj : rs) {
			if (DocumentTypeEnum.GIAO_VIEC.equals(obj.getType())) {
				Task task = taskRepository.findByClientIdAndId(BussinessCommon.getClientId(), obj.getObjId());
				if (task != null) {
					obj.setCode(task.getCodeTask());
				}
			}

			if (DocumentTypeEnum.VAN_BAN_DEN.equals(obj.getType())) {
				Documents docIn = docInRepository.findByClientIdAndId(BussinessCommon.getClientId(), obj.getObjId());
				if (docIn != null) {
					obj.setCode(docIn.getNumberOrSign());
				}
			}

			if (DocumentTypeEnum.VAN_BAN_DI.equals(obj.getType())) {
				DocumentOut docOut = docOutRepository.findByClientIdAndId(BussinessCommon.getClientId(),
						obj.getObjId());
				if (docOut != null) {
					obj.setCode(docOut.getNumberOrSign());
				}
			}

			if (DocumentTypeEnum.VAN_BAN_NOI_BO.equals(obj.getType())) {
				DocumentInternal docInternal = docInternalRepository.findByClientIdAndId(BussinessCommon.getClientId(),
						obj.getObjId());
				if (docInternal != null) {
					obj.setCode(docInternal.getNumberOrSign());
				}
			}
		}
		return rs;

	}

	public List<Long> getListTagByObjId(Long objId, DocumentTypeEnum type) {
		return objTagRepository.findByClientIdAndObjIdAndTypeAndActiveTrue(BussinessCommon.getClientId(), objId, type)
				.stream().map(ObjectTag::getTagId).collect(Collectors.toList());

	}

	public void setDateObjectTag(ObjectTag obj) {
		if (DocumentTypeEnum.GIAO_VIEC.equals(obj.getType())) {
			Task task = taskRepository.findByClientIdAndId(BussinessCommon.getClientId(), obj.getObjId());
			if (task != null) {
				obj.setIssueDate(task.getCreateDate());
			}
		}

		if (DocumentTypeEnum.VAN_BAN_DEN.equals(obj.getType())) {
			Documents docIn = docInRepository.findByClientIdAndId(BussinessCommon.getClientId(), obj.getObjId());
			if (docIn != null) {
				obj.setIssueDate(docIn.getDateIssued());
			}
		}

		if (DocumentTypeEnum.VAN_BAN_DI.equals(obj.getType())) {
			DocumentOut docOut = docOutRepository.findByClientIdAndId(BussinessCommon.getClientId(),
					obj.getObjId());
			if (docOut != null) {
				obj.setIssueDate(docOut.getDateIssued());
			}
		}

		if (DocumentTypeEnum.VAN_BAN_NOI_BO.equals(obj.getType())) {
			DocumentInternal docInternal = docInternalRepository.findByClientIdAndId(BussinessCommon.getClientId(),
					obj.getObjId());
			if (docInternal != null) {
				obj.setIssueDate(docInternal.getApproveDate());
			}
		}
	}
}
