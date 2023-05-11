package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.Attachment;
import com.vz.backend.business.domain.AttachmentComment;
import com.vz.backend.business.domain.DocumentComment;
import com.vz.backend.business.domain.DocumentInTracking;
import com.vz.backend.business.domain.Documents;
import com.vz.backend.business.dto.FollowDto;
import com.vz.backend.business.repository.IDocumentInTrackingRepository;
import com.vz.backend.business.repository.IDocumentRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentInTrackingEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.ListObjectDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.CategoryService;
import com.vz.backend.core.service.OrganizationService;
import com.vz.backend.core.service.UserService;

@Service
public class DocumentInTrackingService extends BaseService<DocumentInTracking> {

	@Autowired
	IDocumentInTrackingRepository trackingRepository;

	@Autowired
	IDocumentRepository docRepository;

	@Autowired
	UserService uService;

	@Autowired
	CategoryService catService;

	@Autowired
	OrganizationService orgService;

	@Autowired
	DocumentInProcessService processService;
	
	@Autowired
	AttachmentService attService;
	
	@Autowired
	AttachmentCommentService attCmtService;
	
	@Autowired
	DocumentCommentService docCmtService;

	@Override
	public IRepository<DocumentInTracking> getRepository() {
		return trackingRepository;
	}

	/**
	 * data from server
	 *
	 * @param docId
	 * @param action
	 * @param userId
	 * @return
	 */
	public DocumentInTracking save(Long docId, DocumentInTrackingEnum action, Long userId) {
		User user = uService.validUserId(userId);
		Documents doc = docRepository.findByClientIdAndId(user.getClientId(), docId);
		if (doc == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_DOC);
		}
		Category cat = catService.validCatId(doc.getDocTypeId());

		DocumentInTracking tracking = new DocumentInTracking(docId, userId, action, user.getOrgModel().getName(),
				cat.getName());
		return getRepository().save(tracking);
	}

	public DocumentInTracking save(Documents doc, DocumentInTrackingEnum action, Long userId, Long clientId) {
		User user = uService.validUserId(userId, clientId);
		if (doc == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_DOC);
		}
		Category cat = catService.validCatId(doc.getDocTypeId(), clientId);

		DocumentInTracking tracking = new DocumentInTracking(doc.getId(), userId, action, user.getOrgModel().getName(),
				cat.getName());
		return getRepository().save(tracking);
	}

	/**
	 * data from server
	 *
	 * @param docId
	 * @param action
	 * @param userIds
	 * @param main
	 * @return
	 */
	public List<DocumentInTracking> save(Documents doc, DocumentInTrackingEnum action, List<User> users, Long main,
			int type) {
		List<DocumentInTracking> tList = new ArrayList<>();
		if (BussinessCommon.isEmptyList(users)) {
			return Collections.emptyList();
		}
		users.forEach(i -> {
			DocumentInTracking t = new DocumentInTracking(doc.getId(), i.getId(),
					getActionStatus(i.getId(), main, action, type), i.getOrgModel().getName(),
					doc.getDocType().getName());
			tList.add(t);
		});

		return getRepository().saveAll(tList);
	}

	public List<DocumentInTracking> save(List<Documents> docIds, DocumentInTrackingEnum action, User u) {
		if (docIds == null || docIds.isEmpty()) {
			return Collections.emptyList();
		}
		List<DocumentInTracking> trackingList = new ArrayList<>();
		for (Documents d : docIds) {
			DocumentInTracking tracking = new DocumentInTracking(d.getId(), u.getId(), action,
					u.getOrgModel().getName(), d.getDocType() != null ? d.getDocType().getName() : "");
			trackingList.add(tracking);
		}

		return getRepository().saveAll(trackingList);
	}

	private DocumentInTrackingEnum getActionStatus(Long user, Long main, DocumentInTrackingEnum action, int type) {
		if (!user.equals(main)) {
			return action;
		}
		if (type == Constant.TRANSFER_HANDLE_TYPE) {
			return DocumentInTrackingEnum.TRANSFER;
		}
		if (type == Constant.RETURN_DOC_TYPE) {
			return DocumentInTrackingEnum.REJECT;
		}
		return action;
	}

	public ListObjectDto<FollowDto> listFollow(Long docId, Integer page, Documents doc) {
		int offset = page.equals(1) ? 0 : page * Constant.NUMBER_OF_PAGE;
		Sort sort = Sort.by(Direction.DESC, "updateDate", "createDate");
		Page<FollowDto> pList = trackingRepository.findByIdAndClientId(BussinessCommon.getClientId(), docId,
				BussinessCommon.castToPageable(page, sort));
		String transferer = Boolean.TRUE.equals(doc.getMergedLines()) && doc.getPersonEnter() != null ? doc.getPersonEnter().getFullName() : null;
		if (!BussinessCommon.isEmptyPage(pList)) {
			List<FollowDto> tList = pList.getContent();
			for (FollowDto t : tList) {
				t.setNo(++offset);
				t.setAction(t.getActionEnum().getName());
				t.setTransferer(transferer);
			}
		}
		return BussinessCommon.paging(pList);
	}
	
	public DocumentInTracking save(Documents doc, DocumentInTrackingEnum action, User user) {
		try {
			DocumentInTracking tracking = new DocumentInTracking(doc.getId(), user.getId(), action,
					user.getOrgModel().getName(), doc.getDocType().getName());
			return getRepository().save(tracking);
		} catch (Exception e) {
			throw new RestExceptionHandler(Message.ERROR_SYS);
		}
	}
	
	
	public void trackingForDownloadFile(String fileName, int type ) {
		Documents doc;
		if(type == Constant.TYPE_ATT) {
			Attachment att = attService.validFileByName(fileName);
			doc = att.getDocument();
		} else {
			AttachmentComment attCmt = attCmtService.findFileByName(fileName);
			DocumentComment docCmt = docCmtService.findByIdCmt(attCmt.getCommentId());
			doc = docCmt.getDocument();
		}
		
		save(doc, DocumentInTrackingEnum.DOWNLOAD_FILE, BussinessCommon.getUser());
	}

	public List<FollowDto> all(Long docId) {
		return trackingRepository.findByIdAndClientId(BussinessCommon.getClientId(), docId);
	}
	
	/**
	 * Save all tracking by user list
	 * @param docId
	 * @param action
	 * @param docType
	 * @param userIds
	 */
	public void saveAll(Long docId, DocumentInTrackingEnum action, String docType, List<Long> userIds) {
		List<User> users = uService.findByIds(userIds, true);
		for (User i : users) {
			getRepository().save(new DocumentInTracking(docId, i.getId(), action, i.getOrgModel().getName(), docType));
		}
	}

	public List<Long> getUserIdByDocIdAndActions(Long docId, List<DocumentInTrackingEnum> actions) {
		return trackingRepository.getUserIdByDocIdAndActions(BussinessCommon.getClientId(), docId, actions);
	}
}
