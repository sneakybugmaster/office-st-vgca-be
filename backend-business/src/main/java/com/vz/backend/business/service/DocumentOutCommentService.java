package com.vz.backend.business.service;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.DocumentComment;
import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.DocumentOutComment;
import com.vz.backend.business.domain.DocumentOutProcess;
import com.vz.backend.business.dto.document.DocOutCommentDto;
import com.vz.backend.business.repository.IDocumentOutCommentRepository;
import com.vz.backend.business.repository.IDocumentOutRepository;
import com.vz.backend.core.auth.SecurityContext;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentOutHandleStatusEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.CategoryService;
import com.vz.backend.core.service.OrganizationService;
import com.vz.backend.core.service.UserService;
import org.springframework.transaction.annotation.Transactional;

@Service
public class DocumentOutCommentService extends BaseService<DocumentOutComment> {
	@Value("${configs.load-comment-by-org-and-position: false}")
	private boolean loadComment;

	@Autowired
	IDocumentOutCommentRepository docOutCmtRepository;

	@Autowired
	private OrganizationService orgService;

	@Autowired
	IDocumentOutRepository docOutRepo;

	@Autowired
	CategoryService catService;

	@Autowired
	UserService userService;

	@Autowired
	DocumentOutProcessService docOutProcessService;

	@Autowired
	NotificationService notiService;

	@Autowired
	private DocumentOutTrackingService docOutTrackingService;

	@Override
	public IRepository<DocumentOutComment> getRepository() {
		return docOutCmtRepository;
	}

	public void validCommentLength(String cmt) {
		if (cmt != null && cmt.length() > Constant.COMMENT_LENGTH) {
			throw new RestExceptionHandler("Độ dài ý kiến không được quá " + Constant.COMMENT_LENGTH + " ký tự");
		}
	}

	public void validCommentLength(DocumentOutComment input) {
		if (input.getComment() != null && input.getComment().length() > Constant.COMMENT_LENGTH) {
			throw new RestExceptionHandler("Độ dài ý kiến không được quá " + Constant.COMMENT_LENGTH + " ký tự");
		}
	}

	public void validSaveInput(DocumentOutComment input) {
		validCommentLength(input);
		if (input == null || input.getDocId() == null) {
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
		}
		DocumentOut doc = docOutRepo.findByClientIdAndId(BussinessCommon.getClientId(), input.getDocId());
		if (doc == null) {
			throw new RestExceptionHandler(Message.DOCUMENT_NOT_FOUND);
		}
	}

	@Override
	public DocumentOutComment save(DocumentOutComment input) {
		validSaveInput(input);
		User user = SecurityContext.getCurrentUser();
		if (user != null) {
			input.setUserId(user.getId());
			input.setUserPosition(catService.findByClientIdAndId(user.getClientId(), user.getPosition()).getName());
		}
		// If cmt has attachment -> addAttachment(file);
		return docOutCmtRepository.save(input);
	}

	private void checkDocumentStatus(Long docId) {
		List<DocumentStatusEnum> listStatus = Arrays.asList(DocumentStatusEnum.DU_THAO, DocumentStatusEnum.THU_HOI_XL);
		if (Boolean.TRUE.equals(docOutRepo.existsDocumentOutByIdAndStatusIn(docId, listStatus))) {
			throw new RestExceptionHandler("Trạng thái văn bản không cho phép nhập ý kiến.");
		}
	}

	public DocumentOutComment saveCmtAndUpdateStatus(Long docId, String cmt) {
		validCommentLength(cmt);
//		checkDocumentStatus(docId);
		// Update Handle Status on process
		docOutProcessService.updateHandleStatus(docId, BussinessCommon.getUser().getId(),
				DocumentOutHandleStatusEnum.DA_Y_KIEN);

		DocumentOutComment dc = new DocumentOutComment();
		User user = SecurityContext.getCurrentUser();
		dc.setUserId(user.getId());
		dc.setUserPosition(catService.findByClientIdAndId(user.getClientId(), user.getPosition()).getName());
		dc.setDocId(docId);
		dc.setComment(cmt);
		return docOutCmtRepository.save(dc);
	}

	@Transactional
	public DocumentOutComment saveCmtAndUpdateStatus(DocumentOutComment cmt) {

		validCommentLength(cmt);
		
		// Remove check status
//		checkDocumentStatus(cmt.getDocId());
		User user = SecurityContext.getCurrentUser();
		// Update Handle Status on process
		DocumentOutProcess input = docOutProcessService.getLastByDocIdAndUserIdAndHandleStatus(true, cmt.getDocId(),
				user.getId(), DocumentOutHandleStatusEnum.CHO_Y_KIEN);
		if (input != null) {
			// Delete notification
			notiService.setActiveByUserIdAndDocIdAndDocType(user.getId(), cmt.getDocId(), DocumentTypeEnum.VAN_BAN_DI,
					false);
			input.setHandleStatus(DocumentOutHandleStatusEnum.DA_Y_KIEN);
			docOutProcessService.save(input);
		}

		cmt.setUserId(user.getId());
		cmt.setUserPosition(catService.findByClientIdAndId(user.getClientId(), user.getPosition()).getName());
		return docOutCmtRepository.save(cmt);
	}

	public DocumentOutComment saveCmt(Long docId, String cmt) {
		validCommentLength(cmt);
		DocumentOutComment dc = new DocumentOutComment();
		User user = SecurityContext.getCurrentUser();
		dc.setUserId(user.getId());
		dc.setUserPosition(catService.findByClientIdAndId(user.getClientId(), user.getPosition()).getName());
		dc.setDocId(docId);
		dc.setComment(cmt);
		return docOutCmtRepository.save(dc);
	}

	public List<DocOutCommentDto> getListByDocId(Long docId) {
		User user = BussinessCommon.getUser();
		if (loadComment) {
//			List<Long> subOrgs = orgService.findParentAndSubAndSameOrgByCurrOrg(user.getOrg(), true);
//			return docOutCmtRepository.findByCurrOrgAndSubOrgInAndDocIdAndClientIdAndActive(user.getOrg(), subOrgs, user.getClientId(), docId, true);
			Set<Long> listUserId = new HashSet<Long>();
//			List<DocumentOutTracking> listTrack = docOutTrackingService.findTrackingByToUserAndDocId(user.getId(), docId);
			List<Long> fromUser = docOutTrackingService.findFromUserByToUserAndDocId(user.getId(), docId);
			fromUser.add(user.getId());
			List<Long> toUser = docOutTrackingService.findToUserByFromUserInAndDocId(fromUser, docId);
			listUserId.addAll(fromUser);
			listUserId.addAll(toUser);
			List<Long> userXYK = docOutProcessService.findUserXYKByUserIdAndDocId(user.getId(), docId);
			List<Long> userYK = docOutProcessService.findUserYKByCreateByAndDocId(user.getId(), docId);
			listUserId.addAll(userXYK);
			listUserId.addAll(userYK);
			return docOutCmtRepository.findByListUserIdAndDocIdAndClientIdAndActive(listUserId, user.getClientId(), docId, true);
		}
		return docOutCmtRepository.findByClientIdAndDocIdAndActive(user.getClientId(), docId, true);
	}

	public Boolean deleteComment(Long cmtId, boolean showError) {
		Optional<DocumentOutComment> dc = docOutCmtRepository.findById(cmtId);
		if (dc.isPresent()) {
			dc.get().setActive(false);
			docOutCmtRepository.save(dc.get());
		} else {
			if (showError) {
				throw new RestExceptionHandler(Message.ACTION_FAILED);
			}
		}
		return true;
	}

	public void validateDownFile(Long attachId, Long docId) {
		if (!docOutCmtRepository.existAttachIdByUserId(attachId, docId)) {
			throw new RestExceptionHandler(Message.NO_PERMISSION);
		}
	}

	public Long getDocIdById(Long id) {
		return docOutCmtRepository.getDocIdById(id);
	}
	
	public DocumentOutComment load(Long id) {
		return valid(id, Message.CMT_NOT_FOUND);
	}
	
	public DocumentOutComment update(Long id, String comment) {
		DocumentOutComment rs = valid(id, Message.CMT_NOT_FOUND);
		
		if(!rs.getCreateBy().equals(BussinessCommon.getUserId())) {
			throw new RestExceptionHandler(Message.NO_ACTION_PERMISSION);
		}
		BussinessCommon.validLengthData(comment, "Ý kiến xử lý", 2000);
		rs.setComment(comment);
		return docOutCmtRepository.save(rs);
	}
}
