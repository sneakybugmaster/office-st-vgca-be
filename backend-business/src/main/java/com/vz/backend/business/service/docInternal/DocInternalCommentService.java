package com.vz.backend.business.service.docInternal;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.config.DocInternalApproveStatusEnum;
import com.vz.backend.business.domain.documentInternal.DocInternalApprove;
import com.vz.backend.business.domain.documentInternal.DocInternalAttach;
import com.vz.backend.business.domain.documentInternal.DocInternalComment;
import com.vz.backend.business.dto.document.CommentDto;
import com.vz.backend.business.dto.document.DocInCommentDto;
import com.vz.backend.business.repository.docInternal.IDocInternalApproveRepository;
import com.vz.backend.business.repository.docInternal.IDocInternalAttachRepository;
import com.vz.backend.business.repository.docInternal.IDocInternalCommentRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.AttachmentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.util.StringUtils;

@Service
public class DocInternalCommentService extends BaseService<DocInternalComment> {

	@Autowired
	IDocInternalCommentRepository docInternalCommentRepo;
	
	@Autowired
	private IDocInternalAttachRepository docInternalAttachRepo;
	
	@Autowired
	private IDocInternalApproveRepository docInternalApproveRepo;

	@Override
	public IRepository<DocInternalComment> getRepository() {
		return docInternalCommentRepo;
	}

	@Autowired
	private DocInternalService docInternalService;

	public Long add(Long approveId, String comment, DocInternalApproveStatusEnum handleStatus) {
		return docInternalCommentRepo.save(new DocInternalComment(approveId, comment, handleStatus)).getId();
	}

	public List<DocInCommentDto> getListCommentByApproveId(List<Long> listApproveId) {
		return docInternalCommentRepo.getListCommentByApproveId(listApproveId);
	}

	public void validCmt(DocInternalComment doc) {

		if (doc == null || doc.getApproveId() == null || StringUtils.isNullOrEmpty(doc.getComment())) {
			throw new RestExceptionHandler(Message.NO_INPUT_DATA);
		}
		BussinessCommon.validLengthData(doc.getComment(), "Ý kiến xử lý", 2000);
	}

	public DocInternalComment addComment(Long docId, String comment) {
		List<DocInternalApprove> approves = docInternalApproveRepo.findByClientIdAndUserIdOrOrgIdAndDocId(
				BussinessCommon.getClientId(), BussinessCommon.getUserId(), BussinessCommon.getUser().getOrg(), docId);
		if (approves.isEmpty())
			throw new RestExceptionHandler(Message.NOT_FOUND_DOC_INTERNAL_APPROVE);
		
		DocInternalApprove approve = approves.get(0);
		if ((approve.getUserId() != null && !BussinessCommon.getUserId().equals(approve.getUserId())
				|| (approve.getOrgId() != null && !BussinessCommon.getUser().getOrg().equals(approve.getOrgId()))))
			throw new RestExceptionHandler(Message.DOC_INTERNAL_INVALID_USER);

		return docInternalCommentRepo
				.save(new DocInternalComment(approve.getId(), comment, DocInternalApproveStatusEnum.BINH_LUAN));
	}

	public List<DocInCommentDto> findDocInternalComments(Long docInternalId) {

		docInternalService.valid(docInternalId, Message.NOT_FOUND_DOCUMENT_INTERNAL);
		List<CommentDto> cList = docInternalCommentRepo.findAllDocInternalComments(BussinessCommon.getClientId(), docInternalId);

		List<DocInCommentDto> rs = new ArrayList<>();

		for (CommentDto comment : cList) {
			List<DocInternalAttach> docs = docInternalAttachRepo
					.findByClientIdAndDocInternalCommentIdAndAttachTypeAndActiveTrue(BussinessCommon.getClientId(),
							comment.getId(), AttachmentTypeEnum.BINH_LUAN);

			DocInCommentDto dto = new DocInCommentDto(comment);
			dto.setInternalAttachments(docs);
			rs.add(dto);
		}

		return rs;
	}
	
	public Boolean deleteComment(Long cmtId, boolean showError) {
		Optional<DocInternalComment> dc = docInternalCommentRepo.findById(cmtId);
		if (dc.isPresent()) {
			dc.get().setActive(false);
			docInternalCommentRepo.save(dc.get());
		} else {
			if (showError) {
				throw new RestExceptionHandler(Message.ACTION_FAILED);
			}
		}
		return true;
	}
	
	public DocInternalComment save(String comment, Long docId) {
		List<DocInternalApprove> approves = docInternalApproveRepo.findByClientIdAndUserIdOrOrgIdAndDocId(
				BussinessCommon.getClientId(), BussinessCommon.getUserId(), BussinessCommon.getUser().getOrg(), docId);
		if (approves.isEmpty())
			return null;

		DocInternalApprove approve = approves.get(0);
		if ((approve.getUserId() != null && !BussinessCommon.getUserId().equals(approve.getUserId())
				|| (approve.getOrgId() != null && !BussinessCommon.getUser().getOrg().equals(approve.getOrgId()))))
			return null;
		return docInternalCommentRepo.save(
				new DocInternalComment(approve.getId(), comment, DocInternalApproveStatusEnum.BINH_LUAN));
	}
}
