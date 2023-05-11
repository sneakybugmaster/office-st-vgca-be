package com.vz.backend.business.service.docInternal;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import com.vz.backend.business.config.DocInternalHandleEnum;
import com.vz.backend.business.domain.documentInternal.*;
import com.vz.backend.business.dto.document.*;
import com.vz.backend.business.repository.docInternal.IDocInternalCommentRepository;
import com.vz.backend.core.config.*;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.service.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.business.config.DocInternalApproveStatusEnum;
import com.vz.backend.business.config.DocInternalApproveTypeEnum;
import com.vz.backend.business.config.DocInternalTrackingEnum;
import com.vz.backend.business.dto.ReportDocByTypeDto;
import com.vz.backend.business.repository.docInternal.IDocInternalApproveRepository;
import com.vz.backend.business.repository.docInternal.IDocInternalAttachRepository;
import com.vz.backend.business.repository.docInternal.IDocInternalRepository;
import com.vz.backend.business.service.NotificationService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.ResponseMessage;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.UserBasicDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.util.StringUtils;

@Service
public class DocInternalService extends BaseService<DocumentInternal> {
	@Autowired
	private IDocInternalRepository docInternalRepo;
	
	@Autowired
	private DocInternalApproveService docInternalApproveService;
	
	@Autowired
	private DocInternalReceiverService docInternalReceiverService;
	
	@Autowired
	private DocInternalAttachService docInternalAttachService;
	
	@Autowired
	private DocInternalCommentService docInternalCommentService;
	
	@Autowired
	private DocInternalTrackingService docInternalTrackingService;

	@Autowired
	private DocInternalProcessService docInternalProcessService;
	
	@Autowired
	private IDocInternalApproveRepository docInternalApproveRepo;
	
	@Autowired
	private NotificationService notiService;
	
	@Autowired
	private UserService userService;
	
	@Autowired
	private IDocInternalAttachRepository docInternalAttachRepo;

	@Autowired
	private CategoryService categoryService;

	@Autowired
	IDocInternalCommentRepository docInternalCommentRepo;
	
	@Override
	public IRepository<DocumentInternal> getRepository() {
		return docInternalRepo;
	}

	public static final List<DocumentStatusEnum> NOT_RETAKE_STATUS = Arrays.asList(DocumentStatusEnum.NB_THU_HOI,
			DocumentStatusEnum.NB_DU_THAO, DocumentStatusEnum.NB_BAN_HANH);
	
	private static final List<DocumentStatusEnum> DRAFT_STATUS = Arrays.asList(DocumentStatusEnum.NB_THU_HOI,
			DocumentStatusEnum.NB_DU_THAO, DocumentStatusEnum.NB_TRA_LAI);
	
	public DocumentInternal getById(Long id) {
		DocumentInternal result = docInternalRepo.findByClientIdAndId(BussinessCommon.getClientId(), id);
		if (result == null)
			throw new RestExceptionHandler(Message.DOCUMENT_NOT_FOUND);
		return result;
	}
	
	public void validatePermission(Long docId) {
		if (!checkPermission(docId))
			throw new RestExceptionHandler(Message.NO_PERMISSION);
	}
	
	public void validatePermission(DocumentInternal doc) {
		
	}
	
	public boolean checkPermission(Long docId) {
		return docInternalRepo.checkPermission(docId, BussinessCommon.getUserId());
	}
	
	private void validateInput(DocInternalCreateDto input) {
		if (DocumentStatusEnum.NB_DU_THAO.equals(input.getStatus())) {
			if (StringUtils.isNullOrEmpty(input.getPreview()))
				throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
		} else {
			if (StringUtils.isNullOrEmpty(input.getPreview()) || input.getStatus() == null)
				throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
		}
		
	}

	public Long add(DocInternalCreateDto input) {
		validateInput(input);
		validNumberOrSign(input.getNumberOrSign());

		DocumentInternal doc = new DocumentInternal(input);
		doc = docInternalRepo.save(doc);
		// Add người duyệt và phòng ban đồng trình
		List<Long> listUserReceive = new ArrayList<>();
		List<Long> listOrgReceive = new ArrayList<>();
		for (DocInternalReceiver receiver : input.getListReceiver()) {
			if (DocInternalApproveTypeEnum.ORG.equals(receiver.getType())) {
				listOrgReceive.add(receiver.getOrgId());
			}
			if (DocInternalApproveTypeEnum.USER.equals(receiver.getType())) {
				listUserReceive.add(receiver.getUserId());
			}
		}
//		docInternalApproveService.add(doc.getId(), input.getListUserApprove(), input.getListOrgApprove(), input.getListCommenterApprove(),
//				input.getSignerId(), listUserReceive, listOrgReceive);
		// Add người ký
		docInternalApproveService.addSigners(doc.getId(), input.getListSigner());
		// Add nơi nhận
		docInternalReceiverService.add(doc.getId(), input.getListReceiver());
		// Add tracking
		docInternalTrackingService.add(doc.getId(), DocInternalTrackingEnum.CREATE);
		// Add process
//		docInternalProcessService.save();
		// Add notification
		if (DocumentStatusEnum.NB_CHO_DUYET.equals(input.getStatus())) {
			Set<Long> listUserIds = new HashSet<>();
			Set<Long> listUserReceiverIds = new HashSet<>();
			if (input.getListSigner() != null)
				listUserIds.addAll(input.getListSigner());
			
			listUserIds.addAll(userService.findAllLeaderByOrgIdAndClientId(input.getListOrgApprove()));
			
			listUserReceiverIds.addAll(listUserReceive);
			listUserReceiverIds.addAll(userService.findLeadershipByOrgIdIn(listOrgReceive));
			
			notiService.addAll(listUserIds, doc.getId(), doc.getPreview(), DocumentTypeEnum.VAN_BAN_NOI_BO,
					NotificationHandleStatusEnum.NB_CHO_DUYET, ModuleCodeEnum.DOC_INTERNAL_APPROVE);
			notiService.addAll(listUserReceiverIds, doc.getId(), doc.getPreview(), DocumentTypeEnum.VAN_BAN_NOI_BO,
					NotificationHandleStatusEnum.NB_THUC_HIEN, ModuleCodeEnum.DOC_INTERNAL_APPROVE);
		}

		return doc.getId();
	}

	public void validNumberOrSign(String numberOrSign) {
		boolean has = docInternalRepo.findByNumberOrSign(BussinessCommon.getClientId(), numberOrSign, BussinessCommon.getOrgId());
		if(has) throw new RestExceptionHandler(Message.INVALID_NUMBER_SIGN);
	}
	
	public boolean addFile(Long docId, MultipartFile[] fileND, MultipartFile[] filePL) {
		User u = BussinessCommon.getUser();
		DocumentInternal doc = getById(docId);
		// Check permission
		if (!u.getId().equals(doc.getCreateBy()))
			throw new RestExceptionHandler(Message.NO_PERMISSION);
		// Add file văn bản
		docInternalAttachService.add(docId, fileND, AttachmentTypeEnum.VAN_BAN);
		// Add file phụ lục
		if (filePL != null && filePL.length > 0)
			docInternalAttachService.add(docId, filePL, AttachmentTypeEnum.PHU_LUC);
		return true;
	}

	public boolean update(Long id, DocInternalCreateDto input) {
		validateInput(input);

		User u = BussinessCommon.getUser();
		DocumentInternal doc = getById(id);

		if (!doc.getNumberOrSign().equalsIgnoreCase(input.getNumberOrSign()))
			validNumberOrSign(input.getNumberOrSign());

		// Check trạng thái văn bản
		if(!DRAFT_STATUS.contains(doc.getStatus())) {
			throw new RestExceptionHandler(Message.DOCUMENT_STATUS_NOT_ALLOW);
		}
		
		// Check permission
		if (!u.getId().equals(doc.getCreateBy()))
			throw new RestExceptionHandler(Message.NO_PERMISSION);
		// Add thông báo
		if (DocumentStatusEnum.NB_TRA_LAI.equals(doc.getStatus()) || (DocumentStatusEnum.NB_DU_THAO.equals(doc.getStatus()) && DocumentStatusEnum.NB_CHO_DUYET.equals(input.getStatus()))
				|| (DocumentStatusEnum.NB_THU_HOI.equals(doc.getStatus()) && DocumentStatusEnum.NB_CHO_DUYET.equals(input.getStatus()))) {
			Set<Long> listUserIds = new HashSet<>();
			listUserIds.addAll(input.getListSigner());
			listUserIds.addAll(userService.findLeadershipByOrgIdIn(input.getListOrgApprove()));
			notiService.addAll(listUserIds, doc.getId(), doc.getPreview(), DocumentTypeEnum.VAN_BAN_NOI_BO,
					NotificationHandleStatusEnum.NB_CHO_DUYET, ModuleCodeEnum.DOC_INTERNAL_APPROVE);
		}
		// Update
		doc.setNumberOrSign(input.getNumberOrSign());
		doc.setPreview(input.getPreview());
		doc.setDocBookId(input.getDocBookId());
		doc.setDocTypeId(input.getDocTypeId());
		doc.setDocDate(input.getDocDate());
		doc.setSignDate(input.getSignDate());
		doc.setIssuedQuantity(input.getIssuedQuantity());
		doc.setOrgCreateId(input.getOrgCreateId());
		doc.setUrgentId(input.getUrgentId());
		doc.setSecurityId(input.getSecurityId());
		if (DocumentStatusEnum.NB_TRA_LAI.equals(input.getStatus()) || DocumentStatusEnum.NB_THU_HOI.equals(input.getStatus()))
			doc.setStatus(DocumentStatusEnum.NB_CHO_DUYET);
		else
			doc.setStatus(input.getStatus());
		docInternalRepo.save(doc);
		// Update người ký
		docInternalApproveService.updateSigner(doc.getId(), input.getListSigner());
		// Update nơi nhận
		docInternalReceiverService.update(doc.getId(), input.getListReceiver());
		// Add tracking
		docInternalTrackingService.add(id, DocInternalTrackingEnum.UPDATE);
		return true;
	}

	@Transactional
	public boolean updateFile(Long docId, List<Long> deleteIds, MultipartFile[] fileND, MultipartFile[] filePL) {
		User u = BussinessCommon.getUser();
		DocumentInternal doc = getById(docId);
		// Check trạng thái văn bản
//		if(!DRAFT_STATUS.contains(doc.getStatus())) {
//			throw new RestExceptionHandler(Message.DOCUMENT_STATUS_NOT_ALLOW);
//		}
		// Check permission
		if (!u.getId().equals(doc.getCreateBy()))
			throw new RestExceptionHandler(Message.NO_PERMISSION);
		// Delete file
		docInternalAttachService.deactiveByIdIn(deleteIds);
		// Add file văn bản
		docInternalAttachService.add(docId, fileND, AttachmentTypeEnum.VAN_BAN);
		// Add file phụ lục
		if (filePL != null && filePL.length > 0)
			docInternalAttachService.add(docId, filePL, AttachmentTypeEnum.PHU_LUC);
		return true;
	}
	
	public boolean addSignFile(Long docId, MultipartFile[] files) {
		User u = BussinessCommon.getUser();
		getById(docId);
		// Check permission
		DocInternalApprove process = docInternalApproveService.findByDocIdAndUserIdAndHandleStatusAndType(docId, u.getId(), DocInternalApproveStatusEnum.CHO_DUYET, DocInternalApproveTypeEnum.SIGNER);
		if (process == null || !u.getId().equals(process.getUserId()))
			throw new RestExceptionHandler(Message.INVALID_PROCESS);
		// Add file ký
		docInternalAttachService.add(docId, files, AttachmentTypeEnum.DA_KY);
		return true;
	}

	@Transactional
	public boolean delete(Long id) {
		DocumentInternal doc = getById(id);
		// Check trạng thái văn bản
		if(!DRAFT_STATUS.contains(doc.getStatus())) {
			throw new RestExceptionHandler(Message.DOCUMENT_STATUS_NOT_ALLOW);
		}
		if (!BussinessCommon.getUserId().equals(doc.getCreateBy()))
			throw new RestExceptionHandler(Message.DELETE_NOT_ALLOWED);
		doc.setActive(false);
		docInternalRepo.save(doc);
		// Add tracking
		docInternalTrackingService.add(id, DocInternalTrackingEnum.DELETE);
		// Delete thông báo
		notiService.deactiveAllByDocIdAndDocType(id, DocumentTypeEnum.VAN_BAN_NOI_BO);
		return true;
	}

	public DocInternalDetailDto getDetailById(Long id) {
		// Check permission
		validatePermission(id);
		DocumentInternal doc = docInternalRepo.getDetailById(id, BussinessCommon.getClientId());
		if (!BussinessCommon.getUserId().equals(doc.getCreateBy()) && !Boolean.TRUE.equals(doc.getRead())) {
			doc.setRead(true);
			docInternalRepo.save(doc);
		}
		DocInternalDetailDto result = new DocInternalDetailDto(doc);
		result.setListSigner(docInternalApproveService.getListSigner(id));
		// Get danh sách thông tin phê duyệt
		List<ApproverDto> listApprover = docInternalApproveService.getListApproverByDocId(id);
		if (!listApprover.isEmpty()) {
			List<Long> listApproveId = new ArrayList<>();
			for (ApproverDto approver : listApprover) {
				listApproveId.add(approver.getId());
			}
			List<DocInCommentDto> listCmt = docInternalCommentService.getListCommentByApproveId(listApproveId);
			Long creatorId = docInternalApproveRepo.findCreatorByDocId(BussinessCommon.getClientId(), id);
			if (BussinessCommon.getUserId().equals(creatorId)) {
				for (DocInCommentDto cmt : listCmt) {
					List<DocInternalAttach> docs = docInternalAttachRepo
							.findByClientIdAndDocInternalCommentIdAndAttachTypeAndActiveTrue(BussinessCommon.getClientId(),
									cmt.getId(), AttachmentTypeEnum.BINH_LUAN);
					cmt.setInternalAttachments(docs);
					for (ApproverDto approver : listApprover) {
						if (approver.getId().equals(cmt.getApproveId()))
							approver.getListCmt().add(cmt);
					}
				}
			}
			else {
				for (DocInCommentDto cmt : listCmt) {
					List<DocInternalAttach> docs = docInternalAttachRepo
							.findByClientIdAndDocInternalCommentIdAndAttachTypeAndActiveTrue(BussinessCommon.getClientId(),
									cmt.getId(), AttachmentTypeEnum.BINH_LUAN);
					cmt.setInternalAttachments(docs);
					for (ApproverDto approver : listApprover) {
						if (approver.getId().equals(cmt.getApproveId()) && (approver.getOrgId() == null
								|| (approver.getUserId() == null)))
							approver.getListCmt().add(cmt);
					}
				}
			}
		}
				
		result.setListApprover(listApprover);
		// Get danh sách thông tin trả lại
		result.setListReturn(docInternalTrackingService.getListReturnComment(id));
		List<DocInternalReceiverDto> docInternalReceiverDtos = docInternalReceiverService.getListReceiverByDocId(id);
		if (docInternalReceiverDtos != null && docInternalReceiverDtos.size() >0) {
			for (DocInternalReceiverDto internalReceiverDto: docInternalReceiverDtos) {
//				if (internalReceiverDto.getHandleStatus().equals(DocInternalHandleEnum.EXECUTE)) {
				if (internalReceiverDto.getHandleStatus().equals(DocInternalHandleEnum.values())) {
					DocInternalApprove approverReceiver = docInternalApproveService.findByDocIdAndUserIdAndTypeReceiverAndClientId(id , internalReceiverDto.getUserId());
					List<Long> listApproveIdReceiver = new ArrayList<>();
					if (approverReceiver != null && approverReceiver.getId() != null) {
						listApproveIdReceiver.add(approverReceiver.getId());
						List<DocInCommentDto> listCmt = docInternalCommentService.getListCommentByApproveId(listApproveIdReceiver);
						if (listCmt != null && listCmt.size() > 0) {
							for (DocInCommentDto cmt : listCmt) {
								List<DocInternalAttach> docs = docInternalAttachRepo
										.findByClientIdAndDocInternalCommentIdAndAttachTypeAndActiveTrue(BussinessCommon.getClientId(),
												cmt.getId(), AttachmentTypeEnum.BINH_LUAN);
								cmt.setInternalAttachments(docs);
							}
							internalReceiverDto.setListCmt(listCmt);
						}
					}
				}
			}
		}
		// Get danh sách nơi nhận
		result.setListReceiver(docInternalReceiverDtos);

		// Get danh sách đính kèm
		result.setListAttachment(docInternalAttachService.getListAttachByDocId(id));
		
		result.setCanRetake(canRetake(doc));
		
		// Add tracking
		docInternalTrackingService.add(id, DocInternalTrackingEnum.READ);
		return result;
	}

	@Transactional
	public Long approve(Long docId, boolean accept, String comment, MultipartFile[] files) {
		if (StringUtils.isNullOrEmpty(comment.trim()))
				throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
		User u = BussinessCommon.getUser();
		Long commentId = null;
		boolean isLeadership = Boolean.TRUE.equals(u.getPositionModel().getIsLeadership());
		DocumentInternal doc = getById(docId);
		if (!(DocumentStatusEnum.NB_CHO_DUYET.equals(doc.getStatus())
				|| DocumentStatusEnum.NB_LANH_DAO_KY.equals(doc.getStatus()))) {
			throw new RestExceptionHandler(Message.DOCUMENT_STATUS_NOT_ALLOW);
		}
		// Get list process
		List<DocInternalApprove> listProcess = docInternalApproveService.findByDocIdAndHandleStatusInAndClientId(docId, Arrays.asList(DocInternalApproveStatusEnum.CHO_DUYET), u.getClientId());
		List<DocInternalApprove> approveProcess = new ArrayList<>(); // Danh sách duyệt (tất cả duyệt hết thì lãnh đạo mới ký được).
		List<DocInternalApprove> ldkProcess = new ArrayList<>(); // Lãnh đạo ký
		int countNotApprove = 0; // Khi tất cả mọi người đều duyệt, thì văn bản sẽ chuyển sang trạng thái Chờ lãnh đạo ký.
		for (DocInternalApprove process : listProcess) {
			if (DocInternalApproveTypeEnum.SIGNER.equals(process.getType())) {
				ldkProcess.add(process);
				continue;
			}
			if (u.getId().equals(process.getUserId()) || (u.getOrg().equals(process.getOrgId()) && isLeadership)) {
				approveProcess.add(process);
			} else {
				countNotApprove++;
			}
		}
		DocInternalApproveStatusEnum pStatus = DocInternalApproveStatusEnum.DA_DUYET;
		DocInternalApproveStatusEnum cmtStatus = DocInternalApproveStatusEnum.DA_DUYET;
		DocInternalTrackingEnum action = DocInternalTrackingEnum.DONG_Y_DUYET;
		// Lãnh đạo ký
		if (DocumentStatusEnum.NB_LANH_DAO_KY.equals(doc.getStatus())) {
			if (ldkProcess.size() != 1)
				throw new RestExceptionHandler("OMG... Something wrong here........");
			DocInternalApprove process = ldkProcess.get(0); // docInternalApproveService.findByDocIdAndUserIdAndHandleStatusAndType(docId, u.getId(), DocInternalApproveStatusEnum.CHO_DUYET, DocInternalApproveTypeEnum.SIGNER);
			if (!u.getId().equals(process.getUserId()))
				throw new RestExceptionHandler(Message.INVALID_PROCESS);
			if (!accept) {
				pStatus = DocInternalApproveStatusEnum.CHO_DUYET;
				action = DocInternalTrackingEnum.TU_CHOI_DUYET;
				doc.setStatus(DocumentStatusEnum.NB_TRA_LAI);
				cmtStatus = DocInternalApproveStatusEnum.TRA_LAI;
			} else {
				doc.setStatus(DocumentStatusEnum.NB_BAN_HANH);
				doc.setApproveDate(new Date());
			}
			docInternalRepo.save(doc);
			// Update process
			process.setLastComment(comment);
			process.setHandleStatus(pStatus);
			commentId = docInternalCommentService.add(process.getId(), comment, cmtStatus);
			docInternalApproveService.save(process);
		} else { // Duyệt văn bản
			if (approveProcess.isEmpty())
				throw new RestExceptionHandler(Message.INVALID_PROCESS);
			// Update doc status
			if (!accept) {
				pStatus = DocInternalApproveStatusEnum.CHO_DUYET;
				action = DocInternalTrackingEnum.TU_CHOI_DUYET;
				doc.setStatus(DocumentStatusEnum.NB_TRA_LAI);
				cmtStatus = DocInternalApproveStatusEnum.TRA_LAI;
				docInternalRepo.save(doc);
			} else {
				// Tất cả mọi người đều đã duyệt thì chuyển trạng thái văn bản sang Chờ lãnh đạo ký (Nếu có, không thì chuyển thành ban hành).
				if (countNotApprove == 0) {
					if(doc.getSignerId() == null) {
						doc.setStatus(DocumentStatusEnum.NB_BAN_HANH);
						doc.setApproveDate(new Date());
					} else {
						doc.setStatus(DocumentStatusEnum.NB_LANH_DAO_KY);
						docInternalRepo.save(doc);
						Long signerId = docInternalApproveService.findSignerIdByDocId(doc.getId());
						notiService.add(signerId, doc.getId(), doc.getPreview(), DocumentTypeEnum.VAN_BAN_NOI_BO,
								NotificationHandleStatusEnum.NB_CHO_DUYET, ModuleCodeEnum.DOC_INTERNAL_APPROVE);
					}
				}
			}
			// Update process
			for (DocInternalApprove process : approveProcess) {
				process.setLastComment(comment);
				process.setHandleStatus(pStatus);
				commentId = docInternalCommentService.add(process.getId(), comment, cmtStatus);
			}
			docInternalApproveService.saveAll(approveProcess);
		}
		// Add tracking
		docInternalTrackingService.add(docId, action, commentId);
		// Add notification
		if (!accept) {
			// Delete thông báo
			notiService.deactiveAllByDocIdAndDocType(docId, DocumentTypeEnum.VAN_BAN_NOI_BO);
			// Add thông báo
			notiService.add(doc.getCreateBy(), docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_NOI_BO,
				NotificationHandleStatusEnum.NB_TU_CHOI, ModuleCodeEnum.DOC_INTERNAL_RETURN);
			// Set lại handle status
			docInternalApproveService.updateStatusByDocId(DocInternalApproveStatusEnum.CHO_DUYET, docId);
		} else {
			// Delete thông báo
			notiService.setActiveByUserIdAndDocIdAndDocType(u.getId(), docId, DocumentTypeEnum.VAN_BAN_NOI_BO, false);
		}
		return commentId;
	}

	// tab: 1 - Chờ duyệt; 2 - Đang duyệt; 3 - Trả lại; 4 - Đã ban hành; 5 - Dự thảo; 6 - Đang duyệt; 7 - Đã ban hành
	public Page<DocInternalForListDto> getListDocInternal(int tab, String numberOrSign, String preview,
			String personEnter, Date createDate, Date createFrom, Date createTo, Date approveDate, Date approveFrom, Date approveTo, Pageable pageable) {
		User u = BussinessCommon.getUser();
		List<DocInternalApproveStatusEnum> pStatus = new ArrayList<>();
		List<DocumentStatusEnum> dStatus = new ArrayList<>();
		Long createBy = null;
		Long checkInternal = null;
		switch (tab) { // Fix cái này thì nhớ fix ở report :D
		case 1: // Chờ duyệt
			dStatus.add(DocumentStatusEnum.NB_CHO_DUYET);
			dStatus.add(DocumentStatusEnum.NB_LANH_DAO_KY);
			pStatus.add(DocInternalApproveStatusEnum.CHO_DUYET);
			break;
		case 2: // Đang duyệt
			createBy = u.getId();
			dStatus.add(DocumentStatusEnum.NB_CHO_DUYET);
			dStatus.add(DocumentStatusEnum.NB_LANH_DAO_KY);
			pStatus.add(DocInternalApproveStatusEnum.DA_DUYET);
			break;
		case 3: // Trả lại
			createBy = u.getId();
			pStatus.add(DocInternalApproveStatusEnum.CHO_DUYET);
			dStatus.add(DocumentStatusEnum.NB_TRA_LAI);
			break;
		case 4: // Đã ban hành
			createBy = u.getId();
			checkInternal = 2L;
			dStatus.add(DocumentStatusEnum.NB_BAN_HANH);
			dStatus.add(DocumentStatusEnum.NB_HOAN_THANH);
			pStatus.add(DocInternalApproveStatusEnum.DA_DUYET);
			dStatus.add(DocumentStatusEnum.NB_CHO_DUYET);
			break;
		case 5: // Dự thảo
			createBy = u.getId();
			dStatus.add(DocumentStatusEnum.NB_DU_THAO);
			dStatus.add(DocumentStatusEnum.NB_THU_HOI);
			break;
		case 6: // Đã đăng ký
			createBy = u.getId();
			dStatus.add(DocumentStatusEnum.NB_CHO_DUYET);
			dStatus.add(DocumentStatusEnum.NB_TRA_LAI);
			dStatus.add(DocumentStatusEnum.NB_LANH_DAO_KY);
//			dStatus.add(DocumentStatusEnum.NB_BAN_HANH);
			break;
		case 8: // Văn bản xử lý
			createBy = u.getId();
			checkInternal = 1L;
			dStatus.add(DocumentStatusEnum.NB_BAN_HANH);
			pStatus.add(DocInternalApproveStatusEnum.DA_DUYET);
			break;
		case 9: // Văn bản xử lý hoàn thành
			createBy = u.getId();
			dStatus.add(DocumentStatusEnum.NB_HOAN_THANH);
			pStatus.add(DocInternalApproveStatusEnum.DA_DUYET);
			break;
		default: // Đã ban hành
			createBy = u.getId();
			dStatus.add(DocumentStatusEnum.NB_BAN_HANH);
			break;
		}
		Page<DocInternalForListDto>  dataList =  docInternalRepo.getListDocInternal(u.getId(), createBy, pStatus, dStatus, numberOrSign, preview, personEnter,
					createDate, createFrom, createTo, approveDate, approveFrom, approveTo, u.getClientId(), Arrays.asList(DocumentStatusEnum.NB_LANH_DAO_KY, DocumentStatusEnum.NB_BAN_HANH), DocInternalApproveTypeEnum.SIGNER, checkInternal, pageable);
			for (DocInternalForListDto data: dataList.getContent()
			) {
				data.setCanDocinternal(findByExecuteDocinternal(data.getId()));
			}
			return dataList;
	}

	public Page<DocInternalForListDto> getAllDocInternal(Long tab, String numberOrSign, DocumentStatusEnum status, String preview, String personEnter,
			Date createDate, Date createFrom, Date createTo, Date approveDate, Date approveFrom, Date approveTo, Pageable pageable) {
		User u = BussinessCommon.getUser();
		Long createBy = u.getId();
		if (tab == 7) {
			Category category = categoryService.findByName("Trưởng phòng");
			Long managerId = category.getId();
			return docInternalRepo.getAllDocInternalWithTab7(managerId, u.getId(), createBy, numberOrSign, status, preview, personEnter,
					createDate, createFrom, createTo, approveDate, approveFrom, approveTo, u.getClientId(), Arrays.asList(DocumentStatusEnum.NB_LANH_DAO_KY, DocumentStatusEnum.NB_BAN_HANH), DocInternalApproveTypeEnum.SIGNER, pageable);
		}
		return docInternalRepo.getAllDocInternal(u.getId(), createBy, numberOrSign, status, preview, personEnter,
				createDate, createFrom, createTo, approveDate, approveFrom, approveTo, u.getClientId(), Arrays.asList(DocumentStatusEnum.NB_LANH_DAO_KY, DocumentStatusEnum.NB_BAN_HANH), DocInternalApproveTypeEnum.SIGNER, pageable);
	}

	public Page<DocInternalForListDto> getAllDocComplete(Pageable pageable) {
		List<DocumentStatusEnum> dStatus = new ArrayList<>();
		dStatus.add(DocumentStatusEnum.NB_BAN_HANH);
		return docInternalRepo.getAllDocComplete(BussinessCommon.getUserId(),BussinessCommon.getClientId(),dStatus, pageable);
	}

	public ReportDocByTypeDto report() {
		ReportDocByTypeDto result = new ReportDocByTypeDto();
		User u = BussinessCommon.getUser();
		for (int i = 1; i < 8; i++) {
			List<DocInternalApproveStatusEnum> pStatus = new ArrayList<>();
			List<DocumentStatusEnum> dStatus = new ArrayList<>();
			Long createBy = null;
			List<DocumentStatusEnum> ldStatus = Arrays.asList(DocumentStatusEnum.NB_LANH_DAO_KY, DocumentStatusEnum.NB_BAN_HANH);
			switch (i) {
			case 1: // Chờ duyệt
				dStatus.add(DocumentStatusEnum.NB_CHO_DUYET);
				dStatus.add(DocumentStatusEnum.NB_LANH_DAO_KY);
				pStatus.add(DocInternalApproveStatusEnum.CHO_DUYET);
				result.setInternalWaiting(docInternalRepo.report(u.getId(), createBy, pStatus, dStatus, u.getClientId(), ldStatus, DocInternalApproveTypeEnum.SIGNER));
				break;
			case 2: // Đang duyệt
				createBy = u.getId();
				dStatus.add(DocumentStatusEnum.NB_CHO_DUYET);
				dStatus.add(DocumentStatusEnum.NB_LANH_DAO_KY);
				pStatus.add(DocInternalApproveStatusEnum.DA_DUYET);
				result.setInternalDoing(docInternalRepo.report(u.getId(), createBy, pStatus, dStatus, u.getClientId(), ldStatus, DocInternalApproveTypeEnum.SIGNER));
				break;
			case 3: // Trả lại
				createBy = u.getId();
				pStatus.add(DocInternalApproveStatusEnum.CHO_DUYET);
				dStatus.add(DocumentStatusEnum.NB_TRA_LAI);
				result.setInternalReturn(docInternalRepo.report(u.getId(), createBy, pStatus, dStatus, u.getClientId(), ldStatus, DocInternalApproveTypeEnum.SIGNER));
				break;
			case 4: // Đã ban hành
				createBy = u.getId();
				dStatus.add(DocumentStatusEnum.NB_BAN_HANH);
				dStatus.add(DocumentStatusEnum.NB_HOAN_THANH);
				pStatus.add(DocInternalApproveStatusEnum.DA_DUYET);
				result.setInternalPublish(docInternalRepo.report(u.getId(), createBy, pStatus, dStatus, u.getClientId(), ldStatus, DocInternalApproveTypeEnum.SIGNER));
				break;
			case 5: // Đăng ký dự thảo
				createBy = u.getId();
				dStatus.add(DocumentStatusEnum.NB_DU_THAO);
				dStatus.add(DocumentStatusEnum.NB_THU_HOI);
				result.setInternalRegister(docInternalRepo.report(u.getId(), createBy, pStatus, dStatus, u.getClientId(), ldStatus, DocInternalApproveTypeEnum.SIGNER));
				break;
			case 6: // Chờ duyệt
				dStatus.add(DocumentStatusEnum.NB_CHO_DUYET);
				dStatus.add(DocumentStatusEnum.NB_LANH_DAO_KY);
				pStatus.add(DocInternalApproveStatusEnum.CHO_DUYET);
				result.setInternalApprove(docInternalRepo.report(u.getId(), createBy, pStatus, dStatus, u.getClientId(), ldStatus, DocInternalApproveTypeEnum.SIGNER));
				break;
			case 7 : //văn bản xử lý
				createBy = u.getId();
				dStatus.add(DocumentStatusEnum.NB_BAN_HANH);
				pStatus.add(DocInternalApproveStatusEnum.DA_DUYET);
				result.setInternalPendding(docInternalRepo.reportVBXL(u.getId(), createBy, pStatus, dStatus, u.getClientId(), Arrays.asList(DocumentStatusEnum.NB_LANH_DAO_KY, DocumentStatusEnum.NB_BAN_HANH), DocInternalApproveTypeEnum.SIGNER));
				break;
			default:
				break;
			}
		}
		return result;
	}

	public ResponseMessage getNumberOrSign() {
		User u = BussinessCommon.getUser();
		Long number = docInternalRepo.countTotalDocByOrgIdAndClientId(u.getOrg(), u.getClientId()) + 1;
		return new ResponseMessage(number.toString() + Constant.LINK_SIGN + StringUtils.castToAcronym(u.getOrgModel().getName()));
	}

	@Transactional
	public Boolean retake(Long id) {
		DocumentInternal doc = valid(id, Message.DOCUMENT_NOT_FOUND);
		if (!canRetake(doc))
			throw new RestExceptionHandler(Message.NO_ACTION_PERMISSION);
		doc.setStatus(DocumentStatusEnum.NB_THU_HOI);
		docInternalRepo.save(doc);
		docInternalApproveService.inActiveByDocId(id);
		docInternalReceiverService.inActiveByDocId(id);
		docInternalTrackingService.add(id, DocInternalTrackingEnum.THU_HOI_BH);
		notiService.deactiveAllByDocIdAndDocType(id, DocumentTypeEnum.VAN_BAN_NOI_BO);
		return true;
	}
	
	/**
	 * for create can retake document internal
	 * @param doc
	 */
	public boolean canRetake(DocumentInternal doc) {
		return BussinessCommon.getUserId().equals(doc.getCreateBy()) && !Boolean.TRUE.equals(doc.getRead())
				&& !NOT_RETAKE_STATUS.contains(doc.getStatus());
	}
	
	public List<Long> getRelatedByDocId(Long docId) {
		List<Long> rs = new ArrayList<>();
		DocumentInternal doc = valid(docId, Message.NOT_FOUND_DOC);
		rs.add(doc.getCreateBy());
		
		// Người duyệt
		List<DocInternalApprove> approves = docInternalApproveService.findByDocId(docId);
		List<Long> aprroveIds = approves.stream().map(DocInternalApprove::getUserId).collect(Collectors.toList());
		
		// Người nhận
		List<DocInternalReceiver> receives = docInternalReceiverService.findByDocId(docId);
		List<Long> receiveIds = receives.stream().map(DocInternalReceiver::getUserId).collect(Collectors.toList());
		
		// Người kí
		List<UserBasicDto> singerIds = userService.findLanhDaoKy();
		List<Long> singerId = singerIds.stream().map(UserBasicDto::getId).collect(Collectors.toList());
		
		rs.addAll(aprroveIds);
		rs.addAll(receiveIds);
//		rs.addAll(singerId);
		return rs;
	}

	@Transactional
	public Long completeNotApprove(Long docId, MultipartFile[] file, String comment) {
		User u = BussinessCommon.getUser();
		DocumentInternal doc = getById(docId);
		if (!DocumentStatusEnum.NB_CHO_DUYET.equals(doc.getStatus()) && !DocumentStatusEnum.NB_BAN_HANH.equals(doc.getStatus())){
			throw new RestExceptionHandler(Message.DOCUMENT_STATUS_NOT_ALLOW);
		}
		if ((comment != null && !comment.equals("")) || (file != null && file.length > 0)) {
			DocInternalApprove docInternalApprove = new DocInternalApprove();
			Boolean checkReciver = docInternalRepo.findByExecuteDocinternal(BussinessCommon.getClientId(), BussinessCommon.getUserId(), docId);
			if (docInternalApproveRepo.findByDocIdAndUserIdAndTypeReceiverAndClientId(docId, BussinessCommon.getUserId(),  BussinessCommon.getClientId()) == null && checkReciver == false) {
				throw new RestExceptionHandler(Message.ACTION_STATUS);
			}
			else if (docInternalApproveRepo.findByDocIdAndUserIdAndTypeReceiverAndClientId(docId, BussinessCommon.getUserId(),  BussinessCommon.getClientId()) == null && checkReciver == true){
				docInternalApprove.setDocId(docId);
				docInternalApprove.setType(DocInternalApproveTypeEnum.RECEIVER);
				docInternalApprove.setUserId(BussinessCommon.getUserId());
				docInternalApprove.setHandleStatus(DocInternalApproveStatusEnum.DA_DUYET);
				docInternalApproveRepo.save(docInternalApprove);
			} else if (docInternalApproveRepo.findByDocIdAndUserIdAndTypeReceiverAndClientId(docId, BussinessCommon.getUserId(),  BussinessCommon.getClientId()) != null ) {
				docInternalApprove = docInternalApproveRepo.findByDocIdAndUserIdAndTypeReceiverAndClientId(docId, BussinessCommon.getUserId(),  BussinessCommon.getClientId());
			}
			docInternalApprove.setLastComment(comment);
			docInternalApproveRepo.save(docInternalApprove);
			DocInternalComment docInternalComment = new DocInternalComment();
			docInternalComment.setComment(comment);
			docInternalComment.setApproveId(docInternalApprove.getId());
			docInternalComment.setHandleStatus(DocInternalApproveStatusEnum.DA_DUYET);
			docInternalCommentRepo.save(docInternalComment);
			docInternalAttachService.addListAttachmentComment(file, docInternalComment.getId(), true);
		}
		if (doc.getSignerId() == null){
			if (DocumentStatusEnum.NB_CHO_DUYET.equals(doc.getStatus())) {
				doc.setStatus(DocumentStatusEnum.NB_BAN_HANH);
			} else {
				boolean check = findByExecuteDocinternal(docId);
				if (!check) throw new RestExceptionHandler(Message.NO_DONE_PROCESS);
				doc.setStatus(DocumentStatusEnum.NB_HOAN_THANH);
			}
			doc.setApproveDate(new Date());
		}
		notiService.setActiveByUserIdAndDocIdAndDocType(u.getId(), docId, DocumentTypeEnum.VAN_BAN_NOI_BO, false);
		return docId;
	}

	public boolean findByExecuteDocinternal(Long docId) {
		boolean checkExecute = docInternalRepo.findByExecuteDocinternal(BussinessCommon.getClientId(), BussinessCommon.getUserId(), docId);
		return checkExecute;
	}
}
