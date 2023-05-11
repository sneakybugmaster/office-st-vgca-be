package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.AttachmentVersion;
import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.DocumentOutAttachment;
import com.vz.backend.business.domain.DocumentOutTracking;
import com.vz.backend.business.dto.FollowDto;
import com.vz.backend.business.repository.IAttachmentVersionRepository;
import com.vz.backend.business.repository.IDocumentOutRepository;
import com.vz.backend.business.repository.IDocumentOutTrackingRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentOutTrackingEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.dto.ListObjectDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.CategoryService;
import com.vz.backend.core.service.OrganizationService;
import com.vz.backend.core.service.UserRoleService;
import com.vz.backend.core.service.UserService;
import com.vz.backend.util.StringUtils;

@Service
public class DocumentOutTrackingService {

	@Autowired
	IDocumentOutTrackingRepository docOutTrackingRepo;

	@Autowired
	OrganizationService orgService;

	@Autowired
	CategoryService catService;

	@Autowired
	UserService userService;

	@Autowired
	UserRoleService userRoleService;

	@Autowired
	IDocumentOutRepository docOutRepo;

	@Autowired
	DocumentOutService docOutService;
	
	@Autowired
	private DocumentOutCommentService docOutCmtService;

	@Autowired
	private IAttachmentVersionRepository attachVersionRepository;
	
	@Autowired
	private DocumentOutAttachmentService docOutAttachService;

	public DocumentOutTracking save(DocumentOutTracking input) {
		input.setOrgName(BussinessCommon.getUser().getOrgModel().getName()); // setOrg
		return docOutTrackingRepo.save(input);
	}
	
	public DocumentOutTracking save(Long docId, String orgName, DocumentOutTrackingEnum action) {
		DocumentOutTracking input = save(docId, action);
		input.setOrgName(orgName); // setOrg
		return docOutTrackingRepo.save(input);
	}

	public DocumentOutTracking save(Long docId, DocumentOutTrackingEnum action) {
		DocumentOutTracking input = new DocumentOutTracking();
		input.setDocId(docId);
		input.setAction(action);
		input.setOrgName(BussinessCommon.getUser().getOrgModel().getName()); // setOrg
		return docOutTrackingRepo.save(input);
	}

	public DocumentOutTracking save(Long docId, Long fromUserId, Long toUserId, DocumentOutTrackingEnum action) {
		DocumentOutTracking input = new DocumentOutTracking();
		input.setDocId(docId);
		if (fromUserId != null) {
			input.setFromUserId(fromUserId);
		}
		if (toUserId != null) {
			input.setToUserId(toUserId);
		}
		input.setAction(action);
		input.setOrgName(BussinessCommon.getUser().getOrgModel().getName()); // setOrg
		return docOutTrackingRepo.save(input);
	}
	
	public DocumentOutTracking save(Long docId, Long fromUserId, Long toUserId, String fileName, DocumentOutTrackingEnum action) {
		DocumentOutTracking input = new DocumentOutTracking();
		input.setDocId(docId);
		if (fromUserId != null) {
			input.setFromUserId(fromUserId);
		}
		if (toUserId != null) {
			input.setToUserId(toUserId);
		}
		input.setFileName(fileName);
		input.setAction(action);
		input.setOrgName(BussinessCommon.getUser().getOrgModel().getName()); // setOrg
		return docOutTrackingRepo.save(input);
	}

	public DocumentOutTracking update(Long docId, Long fromUserId, Long toUserId, Long handlerId,
			DocumentOutTrackingEnum action) {
		DocumentOutTracking dot = docOutTrackingRepo.findFirstByDocIdAndFromUserIdAndActionAndClientIdOrderByIdDesc(docId, fromUserId,
				DocumentOutTrackingEnum.INCOMING, BussinessCommon.getClientId());
		DocumentOutTracking input;
		if (dot == null) {
			input = new DocumentOutTracking();
			input.setDocId(docId);
			if (fromUserId != null) {
				input.setFromUserId(fromUserId);
			}
			if (toUserId != null) {
				input.setToUserId(toUserId);
			}
			input.setHandlerId(handlerId);
			input.setAction(action);
			input.setOrgName(BussinessCommon.getUser().getOrgModel().getName()); // setOrg
		} else {
			input = dot;
			if (fromUserId != null) {
				input.setFromUserId(fromUserId);
			}
			if (toUserId != null) {
				input.setToUserId(toUserId);
			}
			input.setHandlerId(handlerId);
			input.setAction(action);
		}
		return docOutTrackingRepo.save(input);
	}

	public DocumentOutTracking save(Long docId, DocumentOutTrackingEnum action, Long cmtId) {
//		DocumentOutTracking dot = docOutTrackingRepo.findFirstByDocIdAndFromUserIdAndActionAndClientIdOrderByIdDesc(docId,
//				BussinessCommon.getUserId(), DocumentOutTrackingEnum.INCOMING, BussinessCommon.getClientId());
		DocumentOutTracking input = new DocumentOutTracking();
//		if (dot == null) {
			input.setDocId(docId);
//		} else {
//			input = dot;
//		}
		input.setAction(action);
		if (cmtId != null) {
			input.setCommentId(cmtId);
		}
		input.setOrgName(BussinessCommon.getUser().getOrgModel().getName()); // setOrg
		return docOutTrackingRepo.save(input);
	}

	public DocumentOutTracking save(Long docId, Long fromUserId, Long handlerId, DocumentOutTrackingEnum action,
			Long cmtId) {
		DocumentOutTracking dot = docOutTrackingRepo.findFirstByDocIdAndFromUserIdAndActionAndClientIdOrderByIdDesc(docId,
				BussinessCommon.getUserId(), DocumentOutTrackingEnum.INCOMING, BussinessCommon.getClientId());
		DocumentOutTracking input = new DocumentOutTracking();
		if (dot == null) {
			input.setDocId(docId);
		} else {
			input = dot;
		}
		input.setFromUserId(fromUserId);
		input.setHandlerId(handlerId);
		input.setAction(action);
		if (cmtId != null) {
			input.setCommentId(cmtId);
		}
		input.setOrgName(BussinessCommon.getUser().getOrgModel().getName()); // setOrg
		return docOutTrackingRepo.save(input);
	}

	public ListObjectDto<FollowDto> listTracking(Long docId, Integer page) {
		Page<DocumentOutTracking> pDot = docOutTrackingRepo.getListTracking(docId,
				BussinessCommon.castToPageable(page));

		List<DocumentOutTracking> lDot = pDot.getContent();
		List<FollowDto> fList = new ArrayList<>();

		for (int i = 0; i < lDot.size(); i++) {
			DocumentOutTracking d = lDot.get(i);
			FollowDto f = new FollowDto(d);
			f.setNo((page - 1) * Constant.NUMBER_OF_PAGE + i + 1);
			fList.add(f);
		}
		return new ListObjectDto<>(pDot.getTotalPages(), pDot.getTotalElements(), fList);
	}

	public void validTracking(DocumentOutTracking input) {
		if (input == null || input.getDocId() == null || input.getAction() == null) {
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
		}

		DocumentOut doc = docOutRepo.findByClientIdAndId(BussinessCommon.getClientId(), input.getDocId());
		if (doc == null) {
			throw new RestExceptionHandler(Message.DOCUMENT_NOT_FOUND);
		}
	}

	public DocumentOutTracking getLastTrackingByDocIdAndFromUserIdAndAction(Long docId, Long userId,
			DocumentOutTrackingEnum action) {
		return docOutTrackingRepo.findFirstByDocIdAndFromUserIdAndActionAndClientIdOrderByIdDesc(docId, userId, action, BussinessCommon.getClientId());
	}

	public DocumentOutTracking getLastTrackingByDocIdAndToUserIdAndAction(Long docId, Long userId,
			DocumentOutTrackingEnum action) {
		return docOutTrackingRepo.findFirstByDocIdAndToUserIdAndActionAndClientIdOrderByIdDesc(docId, userId, action, BussinessCommon.getClientId());
	}
	
	public List<Long> findListUserTransfer(Long docId) {
		return docOutTrackingRepo.findListUserTransfer(docId, BussinessCommon.getClientId());
	}

	public Boolean trackDownload(String fileName) {
		String tmpName = StringUtils.decodeFromUrl(fileName);
		Long docId = null;
		Long commentId = null;
		if (fileName.indexOf("_v1") > 0) {
			Optional<AttachmentVersion> oDoa = attachVersionRepository.findByName(fileName);
			if (!oDoa.isPresent()) {
				throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
			}
			AttachmentVersion doa = oDoa.get();
			docId = doa.getDocId();
		} else {
			Optional<DocumentOutAttachment> oDoa = docOutAttachService.findByName(tmpName);
			if (!oDoa.isPresent()) {
				throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
			}
			DocumentOutAttachment doa = oDoa.get();
			docId = doa.getDocId();
			commentId = doa.getCmtId();
		}

		if (docId == null || docId < 1) {
			docId = docOutCmtService.getDocIdById(commentId);
		}
		
		save(docId, BussinessCommon.getUserId(), null, fileName, DocumentOutTrackingEnum.DOWNLOAD_FILE);
		return true;
	}

	public List<DocumentOutTracking> findTrackingByToUserAndDocId(Long toUserId, Long docId) {
		return docOutTrackingRepo.findByDocIdAndToUserIdAndClientId(docId, toUserId, BussinessCommon.getClientId());
	}

	public List<Long> findFromUserByToUserAndDocId(Long toUserId, Long docId) {
		return docOutTrackingRepo.findFromUserByDocIdAndToUserIdAndClientId(docId, toUserId, BussinessCommon.getClientId());
	}

	public List<Long> findToUserByFromUserInAndDocId(List<Long> fromUser, Long docId) {
		return docOutTrackingRepo.findToUserByFromUserInAndDocIdAndClientId(fromUser, docId, BussinessCommon.getClientId());
	}

	public List<FollowDto> all(Long docId) {
		List<DocumentOutTracking> pDot = docOutTrackingRepo.getListTracking(docId, BussinessCommon.getClientId());
		List<FollowDto> rs = new ArrayList<>();
		pDot.forEach(i -> rs.add(new FollowDto(i)));
		return rs;
	}
	
	public DocumentOutTracking update(Long docId, Long fromUserId, Long handlerId, DocumentOutTrackingEnum action) {
		DocumentOutTracking tmp = docOutTrackingRepo.findFirstByDocIdAndHandlerIdAndActionAndClientIdOrderByIdDesc(
				docId, handlerId, DocumentOutTrackingEnum.INCOMING, BussinessCommon.getClientId());
		if (tmp == null) {
			tmp = new DocumentOutTracking();
		}
		tmp.setDocId(docId);
		tmp.setFromUserId(fromUserId);
		tmp.setToUserId(handlerId);
		tmp.setHandlerId(handlerId);
		tmp.setAction(action);
		tmp.setOrgName(BussinessCommon.getUser().getOrgModel().getName()); // setOrg
		return docOutTrackingRepo.save(tmp);
	}

	public void saveAll(Long docId, Long frUserId, List<Long> toUserIds, DocumentOutTrackingEnum action) {
		if (BussinessCommon.isEmptyList(toUserIds))
			return;
		toUserIds.stream().distinct().forEach(i -> update(docId, frUserId, i, action));
	}
}
