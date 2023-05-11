package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.vz.backend.business.repository.IDocumentOutCommentRepository;
import com.vz.backend.core.config.*;
import com.vz.backend.core.domain.Category;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.DocumentOutAttachment;
import com.vz.backend.business.domain.DocumentOutProcess;
import com.vz.backend.business.domain.DocumentReceive;
import com.vz.backend.business.dto.DocumentProcessDto;
import com.vz.backend.business.dto.ReportDocByTypeDto;
import com.vz.backend.business.dto.kpi.KPIDataDto;
import com.vz.backend.business.repository.IDocumentOutAttachmentRepository;
import com.vz.backend.business.repository.IDocumentOutProcessRepository;
import com.vz.backend.business.repository.IDocumentOutRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.ICategoryRepository;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.repository.IUserRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.OrganizationService;
import com.vz.backend.core.service.UserService;
import com.vz.backend.util.StringUtils;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class DocumentOutProcessService extends BaseService<DocumentOutProcess> {

	@Value("${configs.clerical-org: false}")
	private boolean clericalOrg;
	
	@Override
	public IRepository<DocumentOutProcess> getRepository() {
		return docOutProcessRepo;
	}

	@Autowired
	IDocumentOutProcessRepository docOutProcessRepo;

	@Autowired
	IUserRepository userRepository;

	@Autowired
	ICategoryRepository categoryRepository;

	@Autowired
	OrganizationService orgService;

	@Autowired
	IDocumentOutRepository docOutRepo;

	@Autowired
	IDocumentOutAttachmentRepository doaRepo;
	
	@Autowired
	UserService userService;
	
	@Autowired
	DocumentReceiveService drService;
	
	@Autowired
	DocumentOutService docOutService;

	@Autowired
	IDocumentOutCommentRepository documentOutCommentRepository;

	public static DocumentStatusEnum[] DOC_STATUS = new DocumentStatusEnum[] { DocumentStatusEnum.DU_THAO,
			DocumentStatusEnum.BI_TRA_LAI, DocumentStatusEnum.THU_HOI_XL, DocumentStatusEnum.DANG_XU_LY,
			DocumentStatusEnum.CHO_BAN_HANH, DocumentStatusEnum.DA_BAN_HANH };
	private static DocumentOutHandleStatusEnum[] NOT_YET_HANDLE_STATUS = {DocumentOutHandleStatusEnum.CHO_XU_LY, DocumentOutHandleStatusEnum.BI_TRA_LAI};
	
	@Override
	public DocumentOutProcess save(DocumentOutProcess input) {
		log.error("input {}", input.getHandleType());
		input.setOrgName(BussinessCommon.getUser().getOrgModel().getName()); // setOrg
		return docOutProcessRepo.save(input);
	}

	public Page<DocumentProcessDto> findByHandleStatus(DocumentOutHandleStatusEnum[] handleStatus,
			DocumentStatusEnum[] docStatus, String action, Pageable pageable) {
		List<Long> docComment = null;
		List<Long> docNoComment = null;
		if ("waiting-handle".equals(action)) {
			docNoComment = documentOutCommentRepository.getDocIdByUserId(getUserOrListVanThuBan(), BussinessCommon.getClientId(), true);
		}
//		if ("waiting-comment".equals(action)) {
//			docComment = documentOutCommentRepository.getDocIdByUserId(BussinessCommon.getUserId(), BussinessCommon.getClientId(), true);
//		}
		Page<DocumentProcessDto> result = docOutProcessRepo.findByUserIdAndHandleStatusAndDocStatus(
				DocumentTypeEnum.VAN_BAN_DI, getUserOrListVanThuBan(), handleStatus, docStatus, new Date(),
				NOT_YET_HANDLE_STATUS, docComment, docNoComment, pageable);
		DocumentOutProcessService.fillAtt(doaRepo, result.getContent());
		if ("waiting-comment".equals(action) && result != null && result.getContent().size() > 0) {
			for (DocumentProcessDto documentProcessDto: result.getContent()) {
				boolean checkComment = documentOutCommentRepository.existCommentByDocId(BussinessCommon.getClientId(), documentProcessDto.getDocId(), true);
				if (checkComment) {
					documentProcessDto.setStatus(DocumentOutHandleStatusEnum.DA_Y_KIEN.getName());
				}
			}
		}
		return result;
	}

	public Page<DocumentProcessDto> search(DocumentOutHandleStatusEnum[] status, DocumentStatusEnum[] docStatus,
			String text, Pageable pageable) {
		Page<DocumentProcessDto> result = docOutProcessRepo.search(DocumentTypeEnum.VAN_BAN_DI, status, text,
				getUserOrListVanThuBan(), docStatus, new Date(), NOT_YET_HANDLE_STATUS, pageable);
		DocumentOutProcessService.fillAtt(doaRepo, result.getContent());
		return result;
	}

	public Page<DocumentProcessDto> searchAdvance(Boolean important, DocumentOutHandleStatusEnum[] status,
			DocumentStatusEnum[] docStatus,
			String preview, String numberOrSign, Long docTypeId, Long docFieldId, String orgCreateName,
			String userEnter, Date startDate, Date endDate, Boolean sortByImportant, Pageable pageable) {
		Page<DocumentProcessDto> result;
		if (Boolean.TRUE.equals(sortByImportant)) {
			result = docOutProcessRepo.searchAdvanceWithImportant(DocumentTypeEnum.VAN_BAN_DI, important,
					BussinessCommon.getClientId(), status, preview, numberOrSign, docTypeId, docFieldId, orgCreateName,
					userEnter, getUserOrListVanThuBan(), startDate, endDate, docStatus, new Date(), NOT_YET_HANDLE_STATUS, pageable);
		} else {
			result = docOutProcessRepo.searchAdvance(DocumentTypeEnum.VAN_BAN_DI, important,
					BussinessCommon.getClientId(), status, preview, numberOrSign, docTypeId, docFieldId, orgCreateName,
					userEnter, getUserOrListVanThuBan(), startDate, endDate, docStatus, new Date(), NOT_YET_HANDLE_STATUS, pageable);
		}
		DocumentOutProcessService.fillAtt(doaRepo, result.getContent());
		return result;
	}

	public static void fillAtt(IDocumentOutAttachmentRepository doaRepo, List<DocumentProcessDto> content) {
		Map<Long, List<DocumentOutAttachment>> mapAtt = new HashMap<>();
		List<Long> docIds = new ArrayList<>();
		for (DocumentProcessDto dto : content) {
			Long docId = dto.getDocId();
			docIds.add(docId);
			mapAtt.put(docId, new ArrayList<>());
		}
		for (DocumentOutAttachment attachment : doaRepo.findAllByDocIdIn(docIds)) {
			Long docId = attachment.getDocId();
			mapAtt.get(docId).add(attachment);
		}
		for (DocumentProcessDto dto : content) {
			Long docId = dto.getDocId();
			dto.setAttachments(mapAtt.get(docId));
		}
	}

	public DocumentOutProcess save(Long docId, Long userId, Long handlerId, Long delegateUserId,
			DocumentOutHandleStatusEnum status) {
		DocumentOutProcess input = new DocumentOutProcess();
		input.setUserId(userId);
		input.setHandlerId(handlerId);
		input.setDelegateUserId(delegateUserId);
		input.setDocId(docId);
		input.setHandleStatus(status);
		input.setOrgName(BussinessCommon.getUser().getOrgModel().getName()); // setOrg
		return docOutProcessRepo.save(input);
	}

	public DocumentOutProcess save(Long docId, Long userId, Long handlerId, Long delegateUserId, Long delegateId,
			DocumentOutHandleStatusEnum status, Long nodeId) {
		DocumentOutProcess input = new DocumentOutProcess();
		input.setUserId(userId);
		input.setHandlerId(handlerId);
		input.setDelegateUserId(delegateUserId);
		input.setDelegateId(delegateId);
		input.setDocId(docId);
		input.setHandleStatus(status);
		input.setOrgName(BussinessCommon.getUser().getOrgModel().getName()); // setOrg
		input.setNodeId(nodeId);
		return docOutProcessRepo.save(input);
	}
	public DocumentOutProcess saveOrUpdate(Long docId, Long userId, Long handlerId, Long delegateUserId,
			Long delegateId, DocumentOutHandleStatusEnum status, Long nodeId) {
		DocumentOutProcess old = docOutProcessRepo.findFirstByActiveAndDocIdAndUserIdAndClientIdOrderByIdDesc(true,
				docId, userId, BussinessCommon.getClientId());
		DocumentOutProcess input = old != null ? old : new DocumentOutProcess();
		input.setUserId(userId);
		input.setHandlerId(handlerId);
		input.setDelegateUserId(delegateUserId);
		input.setDelegateId(delegateId);
		input.setDocId(docId);
		input.setHandleStatus(status);
		input.setOrgName(BussinessCommon.getUser().getOrgModel().getName()); // setOrg
		input.setNodeId(nodeId);
		input.setRead(false);
		return docOutProcessRepo.save(input);
	}

	public DocumentOutProcess updateHandleStatus(Long docId, Long userId, DocumentOutHandleStatusEnum status) {
		try {
			DocumentOutProcess input = docOutProcessRepo.findFirstByActiveAndDocIdAndUserIdAndClientIdOrderByIdDesc(
					true, docId, userId, BussinessCommon.getClientId());
			input.setHandleStatus(status);
			return docOutProcessRepo.save(input);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	public void validSaveDocOutProcess(DocumentOutProcess input) {
		if (input == null || input.getDocId() == null || input.getHandleStatus() == null) {
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
		}

		DocumentOut doc = docOutRepo.findByClientIdAndId(BussinessCommon.getClientId(), input.getDocId());
		if (doc == null) {
			throw new RestExceptionHandler(Message.DOCUMENT_NOT_FOUND);
		}
	}

	public DocumentOutProcess getLastByDocIdAndUserIdAndHandleStatus(Boolean active, Long docId, Long userId,
			DocumentOutHandleStatusEnum handleStatus) {
		return docOutProcessRepo.findFirstByActiveAndDocIdAndUserIdAndHandleStatusOrderByIdDesc(active, docId, userId,
				handleStatus);
	}

	public List<DocumentOutProcess> getListByDocIdAndUserIdAndHandleStatus(Boolean active, Long docId, Long userId,
			DocumentOutHandleStatusEnum handleStatus) {
		List<DocumentOutProcess> dop = docOutProcessRepo.getByDocIdAndUserIdAndHandleStatus(active, docId, userId,
				handleStatus);
		if (dop == null || dop.isEmpty()) {
			return null;
		}
		return dop;
	}

	public List<DocumentOutProcess> getListByDocIdAndUserIdAndHandleStatus(Boolean active, Long docId, Long userId,
			DocumentOutHandleStatusEnum[] handleStatus) {
		List<DocumentOutProcess> dop = docOutProcessRepo.getByDocIdAndUserIdAndHandleStatus(active, docId, userId,
				handleStatus);
		if (dop == null || dop.isEmpty()) {
			return null;
		}
		return dop;
	}

	public void setActiveByDocIdAndUserIdAndhandleStatus(Boolean active, Long docId, Long userId,
			DocumentOutHandleStatusEnum[] handleStatus) {
		docOutProcessRepo.setActiveByDocIdAndUserIdAndhandleStatus(active, docId, userId, handleStatus);
	}

	public void deactiveByDocId(Long docId) {
		List<DocumentOutProcess> listDOP = docOutProcessRepo.findByDocIdAndClientId(docId,
				BussinessCommon.getClientId());
		if (listDOP != null) {
			for (DocumentOutProcess dop : listDOP) {
				dop.setActive(false);
			}
			docOutProcessRepo.saveAll(listDOP);
		}
	}

	public DocumentOutProcess findFirstByActiveAndDocIdAndUserIdAndClientIdOrderByIdDesc(boolean active, Long docId,
			Long fromUserId, Long clientId) {
		return docOutProcessRepo.findFirstByActiveAndDocIdAndUserIdAndClientIdOrderByIdDesc(active, docId, fromUserId,
				clientId);
	}

	public DocumentOutProcess findFirstByActiveAndDocIdAndUserIdAndHandleStatusOrderByIdDesc(boolean active, Long docId,
			Long fromUserId, DocumentOutHandleStatusEnum handleStatus) {
		return docOutProcessRepo.findFirstByActiveAndDocIdAndUserIdAndHandleStatusOrderByIdDesc(active, docId,
				fromUserId, handleStatus);
	}

	public DocumentOutProcess findFirstByActiveAndDocIdAndUserIdAndHandleStatusInOrderByIdDesc(boolean active,
			Long docId, Long fromUserId, DocumentOutHandleStatusEnum[] handleStatus) {
		return docOutProcessRepo.findFirstByActiveAndDocIdAndUserIdAndHandleStatusInOrderByIdDesc(active, docId,
				fromUserId, handleStatus);
	}

	public DocumentOutProcess findFirstByActiveAndDocIdAndHandleStatusInOrderByIdDesc(boolean active, Long docId,
			DocumentOutHandleStatusEnum[] enums) {
		return docOutProcessRepo.findFirstByActiveAndDocIdAndHandleStatusInOrderByIdDesc(active, docId, enums);
	}
	
	public DocumentOutProcess findFirstByActiveAndDocIdAndUserIdHandleStatusInOrderByIdDesc(boolean active, Long docId, Long userId,
			DocumentOutHandleStatusEnum[] enums) {
		return docOutProcessRepo.findFirstByActiveAndDocIdAndUserIdAndHandleStatusInOrderByIdDesc(active, docId, userId, enums);
	}

	public Long countDocByUser(DocumentOutHandleStatusEnum[] status) {
		User u = BussinessCommon.getUser();
		return docOutProcessRepo.countDocByUser(u.getClientId(), u.getId(), status);
	}

	public List<DocumentOutProcess> findByDocIdAndClientIdLasted(List<Long> docIds) {
		return docOutProcessRepo.findByDocIdAndClientIdLasted(docIds, BussinessCommon.getClientId());
	}

	public List<DocumentOutProcess> findByDocIdAndClientIdLastedAndUser(List<Long> docIds) {
		return docOutProcessRepo.findByDocIdAndClientIdLastedAndUser(docIds, BussinessCommon.getClientId());
	}

	public List<Long> getListUserIdByDocIdAndActive(long docId, Boolean active, long clientId) {
		return docOutProcessRepo.getListUserIdByDocIdAndActiveAndClientId(docId, active, clientId);
	}

	public boolean existUserInProcessByDocIdAndActive(List<Long> userId, Long docId, boolean active, Long clientId) {
		return docOutProcessRepo.existUserInProcessByDocIdAndActiveAndClientId(userId, docId, active, clientId, new Date());
	}

	public boolean existListUserInProcessByDocIdAndActive(List<Long> listUser, Long docId, boolean active, Long clientId) {
		return docOutProcessRepo.existListUserInProcessByDocIdAndActive(listUser, docId, active, clientId);
	}

	public List<DocumentOutProcess> findListProcessByDocIdInAndUserId(List<Long> idList, Long userId) {
		return docOutProcessRepo.findByDocIdInAndUserIdOrderByIdDesc(idList, userId);
	}
	
	public ReportDocByTypeDto reportDocByType() {
		User user = BussinessCommon.getUser();
		List<ReportDocByTypeDto> rsList = docOutProcessRepo.reportDocByType(new Date(), clericalOrg, user.getId(),
				user.getOrg(), user.getClientId());
		return new ReportDocByTypeDto().count(rsList, false);
	}

	public List<Long> findUserXYKByUserIdAndDocId(Long userId, Long docId) {
		return docOutProcessRepo.findUserXYKByUserIdAndDocIdAndHandleStatusIn(userId, docId, Arrays.asList(DocumentOutHandleStatusEnum.CHO_Y_KIEN, DocumentOutHandleStatusEnum.DA_Y_KIEN), BussinessCommon.getClientId());
	}

	public List<Long> findUserYKByCreateByAndDocId(Long userId, Long docId) {
		return docOutProcessRepo.findUserYKByUserIdAndDocIdAndHandleStatusIn(userId, docId, Arrays.asList(DocumentOutHandleStatusEnum.CHO_Y_KIEN, DocumentOutHandleStatusEnum.DA_Y_KIEN), BussinessCommon.getClientId());
	}

	public boolean isNoProcess(Long docId) {
		return docOutProcessRepo.isNoProcess(docId);
	}

	public List<Long> getAllRelateByDocId(Long docId, Long clientId) {
		List<Long> rs = new ArrayList<>();
		DocumentOut doc = docOutRepo.findByClientIdAndId(clientId, docId);
		if (doc == null) {
			return rs;
		}

		List<Long> signerByIds = BussinessCommon.stringToList(doc.getListSignerIds());
		String[] fullNames = new String[0];
		if (!StringUtils.isNullOrEmpty(doc.getListSignersName())) {
			fullNames = doc.getListSignersName().split(",");
		}
		
		List<Long> signerByNames = userService.findUserIdByFullName(fullNames);   
		List<Long> byProcess = docOutProcessRepo.getAllRelate(docId, clientId);
		List<Long> clerical = userService.getListIdsVanThuVBDiByOrg(BussinessCommon.getOrgId());
		
		List<DocumentReceive> receiveIdList = drService.findByClientIdAndDocIdAndType(BussinessCommon.getClientId(), docId, null);
		List<Long> byReceive = docOutService.getUserIdReceiveByDocId(receiveIdList, true);

		rs.addAll(signerByNames);
		rs.addAll(signerByIds);
		rs.addAll(byProcess);
		rs.addAll(clerical);
		rs.addAll(byReceive);

		return rs;
	}

	public List<Long> getAllRelateByDocIdEdit(Long docId, Long clientId) {
		List<Long> rs = new ArrayList<>();
		DocumentOut doc = docOutRepo.findByClientIdAndId(clientId, docId);
		if (doc == null) {
			return rs;
		}

		List<Long> signerByIds = BussinessCommon.stringToList(doc.getListSignerIds());
		String[] fullNames = new String[0];
		if (!StringUtils.isNullOrEmpty(doc.getListSignersName())) {
			fullNames = doc.getListSignersName().split(",");
		}

		List<Long> signerByNames = userService.findUserIdByFullName(fullNames);
		List<Long> byProcess = docOutProcessRepo.getAllRelate(docId, clientId);
		List<Long> clerical = userService.getListIdsVanThuVBDiByOrg(BussinessCommon.getOrgId());

		List<DocumentReceive> receiveIdList = drService.findByClientIdAndDocIdAndType(BussinessCommon.getClientId(), docId, null);
		List<Long> byReceive = docOutService.getUserIdDocumentReceiveByDocId(receiveIdList, true);

		rs.addAll(signerByNames);
		rs.addAll(signerByIds);
		rs.addAll(byProcess);
		rs.addAll(clerical);
		rs.addAll(byReceive);

		return rs;
	}

	public DocumentOutProcess findByUserIdOrDelegateId(Long docId,
													   List<Long> userIds, DocumentOutHandleStatusEnum[] enums) {
		List<DocumentOutProcess> rs = docOutProcessRepo.findByUserRelatedAndDocId(docId, userIds, enums, BussinessCommon.getClientId());
		if(rs.isEmpty()) return null;
		return rs.get(0);
	}

	/**
	 * Find process which userId delegated
	 * @param docId
	 * @param userId
	 * @param status
	 * @return
	 */
	public DocumentOutProcess findByDelegaterAndDocId(Long docId, Long userId,
			DocumentOutHandleStatusEnum status, Boolean active) {
		List<DocumentOutProcess> rs = docOutProcessRepo.findDelegate(docId, userId, status, BussinessCommon.getClientId(), active);
		if(rs.isEmpty()) return null;
		return rs.get(0);
	}
	
	/**
	 * Find process which userId is delegated by delegater
	 * @param docId
	 * @param userId : is delegated
	 * @param delegateUserId : delegater
	 * @param status
	 * @return
	 */
	public DocumentOutProcess findByDelegaterAndDocId(Long docId, Long userId) {
		List<DocumentOutProcess> rs = docOutProcessRepo.findDelegate(docId, userId, BussinessCommon.getClientId());
		if(rs.isEmpty()) return null;
		return rs.get(0);
	}

	public List<KPIDataDto> findAllByToUser(List<Long> userIds, Date startDate, Date endDate) {
		return docOutProcessRepo.findAllByToUser(userIds, BussinessCommon.getClientId(), startDate, endDate);
	}
	
	public List<KPIDataDto> findAllByToUser(Long userId, Date startDate, Date endDate) {
		return docOutProcessRepo.findAllByToUser(userId, BussinessCommon.getClientId(), startDate, endDate);
	}
	
	public List<DocumentOutProcess> findByUserRelatedAndDocId(Long docId, List<Long> userIds) {
		return docOutProcessRepo.findByUserRelatedAndDocId(docId, userIds, BussinessCommon.getClientId());
	}

	public List<Long> knowable(Long userId, Date startDate, Date endDate, boolean done) {
		List<Long> signerDocIds = docOutService.signerDocumentOut();
		return docOutProcessRepo.knowable(userId, startDate, endDate, BussinessCommon.getClientId(), signerDocIds, done);
	}

	private List<Long> getUserOrListVanThuBan() {
		User user = BussinessCommon.getUser();
		Category positionVanThuBan = categoryRepository.findByName(user.getClientId(), Constant.VAN_THU_MAIN);
		List<Long> usersCheck = new ArrayList<>();
		if (positionVanThuBan != null && user.getPositionModel() != null
				&& (user.getPositionModel().getId().longValue() == positionVanThuBan.getId().longValue())) {
			usersCheck = userRepository.getUsersByNameCategory(Constant.VAN_THU_MAIN, user.getOrg());
			if (usersCheck.isEmpty()) {
				throw new RestExceptionHandler(Message.EMPTY_CLERICAL_HANDLE);
			}
		} else {
			usersCheck.add(user.getId());
		}
		return usersCheck;
	}
}
