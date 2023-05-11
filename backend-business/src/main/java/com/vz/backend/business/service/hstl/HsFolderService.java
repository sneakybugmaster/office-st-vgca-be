package com.vz.backend.business.service.hstl;

import java.util.*;
import java.util.stream.Collectors;

import com.vz.backend.business.dto.hstl.*;
import com.vz.backend.core.dto.ListObjectDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Service;

import com.vz.backend.business.config.FolderPermissionEnum;
import com.vz.backend.business.config.FolderTypeEnum;
import com.vz.backend.business.config.HsFolderProcessEnum;
import com.vz.backend.business.config.HsFolderStatusEnum;
import com.vz.backend.business.config.HsFolderTrackingEnum;
import com.vz.backend.business.domain.BusinessTracking;
import com.vz.backend.business.domain.hstl.HsFolder;
import com.vz.backend.business.domain.hstl.HsFolderDocument;
import com.vz.backend.business.domain.hstl.HsFolderFile;
import com.vz.backend.business.domain.hstl.HsFolderProcess;
import com.vz.backend.business.domain.hstl.HsFolderShare;
import com.vz.backend.business.domain.hstl.HsFolderTracking;
import com.vz.backend.business.dto.hstl.ecm.HsFolderRecordListDto;
import com.vz.backend.business.repository.hstl.IHsFolderDocumentRepository;
import com.vz.backend.business.repository.hstl.IHsFolderFileRepository;
import com.vz.backend.business.repository.hstl.IHsFolderProcessRepository;
import com.vz.backend.business.repository.hstl.IHsFolderRepository;
import com.vz.backend.business.repository.hstl.IHsFolderTrackingRepository;
import com.vz.backend.business.service.BusinessTrackingService;
import com.vz.backend.business.service.DocumentOutService;
import com.vz.backend.business.service.DocumentService;
import com.vz.backend.business.service.NotificationService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.AuthorityEnum;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.config.NotificationHandleStatusEnum;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.LabelValueId;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.CategoryService;
import com.vz.backend.core.service.OrganizationService;
import com.vz.backend.core.service.UserService;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import static java.util.Comparator.comparingLong;
import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.toCollection;

@Service
@NoArgsConstructor
@Slf4j
public class HsFolderService extends BaseService<HsFolder> {

	@Autowired
	private IHsFolderRepository hsFolderRepo;

	@Autowired
	private IHsFolderDocumentRepository hsFolderDocumentRepo;

	@Autowired
	private IHsFolderFileRepository hsFolderFileRepo;

	@Autowired
	private IHsFolderProcessRepository hsFolderProcessRepo;

	@Autowired
	private IHsFolderTrackingRepository hsFolderTrackingRepo;

	@Autowired
	private HsFolderShareService hsFolderShareService;

	@Autowired
	private OrganizationService orgService;

	@Autowired
	private NotificationService notifyService;

	@Autowired
	private UserService userService;

	@Autowired
	private DocumentService docService;

	@Autowired
	private DocumentOutService docOutService;

	@Autowired
	private FontService fontService;

	@Autowired
	private BusinessTrackingService btService;

	@Autowired
	private CategoryService catService;

	@Override
	public IRepository<HsFolder> getRepository() {
		return hsFolderRepo;
	}

	public Boolean add(HsFolder input, Long userApprove) {
		input.valids();
		List<HsFolder> olds = getByInfo(input.getTitle(), input.getParentId(), input.getFileCode(), input.getFileNotation());
		if (!olds.isEmpty()) {
			throw new RestExceptionHandler(Message.INVALID_TITLE_FOLDER);
		}

		if (input.getParentId() != null) {
			validatePermission(input.getParentId(), FolderPermissionEnum.FULL, false);
		}
		input.setId(null);
		input.setOrgQLId(BussinessCommon.getOrgId());
		if (userApprove != null) {
			input.setStatus(HsFolderStatusEnum.HS_CONG_VIEC);
		}
		input = hsFolderRepo.save(input);
		//Add permission for creator
		hsFolderShareService.addForCreator(input.getId());
		//increase totalItems for parent folder
		hsFolderRepo.increaseTotalItems(input.getParentId(), 1L);
		//Add tracking
		addTracking(input.getId(), HsFolderTrackingEnum.CREATE, null);
		//Add process
		if (userApprove != null) {
			transferToHSCV(input, userApprove);
		}
		return true;
	}

	public List<HsFolder> getByInfo(String name, Long parentId, String fileCode, String fileNotation) {
		return hsFolderRepo.getByInfo(name, parentId, fileCode, fileNotation, BussinessCommon.getClientId());
	}

	public HsFolder getById(Long folderId) {
		HsFolder folder = findByFolderId(folderId);
		if (folder == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_OBJECT);
		}
		folder.setPageNumber(countTotalPageNumberOfAllDocInFolder(folderId));
		btService.track(folderId, BusinessTracking.BusinessTrackingType.HO_SO);
		return folder;
	}

	public Boolean update(Long id, HsFolder input, Long userApprove) {
		HsFolder folder = findByFolderId(id);

		List<Long> userAuthor =  userService.findUserIdByOrgWithAuthority(BussinessCommon.getOrgId(), AuthorityEnum.DUYET_HOSO);
		if (!BussinessCommon.getUserId().equals(folder.getCreateBy()) && !userAuthor.contains(BussinessCommon.getUserId())) {
			throw new RestExceptionHandler(Message.NO_ACTION_PERMISSION);
		}
		List<HsFolder> olds = getByInfo(input.getTitle(), input.getParentId(), input.getFileCode(), input.getFileNotation());
		List<Long> oldIds = olds.stream().map(HsFolder::getId).collect(Collectors.toList());
		if (!olds.isEmpty() && !oldIds.contains(folder.getId())) {
			throw new RestExceptionHandler(Message.INVALID_TITLE_FOLDER);
		}

		input.valids();
		folder.set(input);
//		folder.setTitle(input.getTitle());
//		folder.setFolderType(input.getFolderType());
//		folder.setMaintenance(input.getMaintenance());
		hsFolderRepo.save(folder);
		//Add tracking
		addTracking(input.getId(), HsFolderTrackingEnum.UPDATE, null);
		if (userApprove != null) {
			List<HsFolderProcessEnum> status = Arrays.asList(HsFolderProcessEnum.PB_CHO_DUYET, HsFolderProcessEnum.PB_TRA_LAI);
			HsFolderProcess process = hsFolderProcessRepo.findFirstByFolderIdAndStatusInAndActiveAndClientIdOrderByIdDesc(id, status, true, BussinessCommon.getClientId());
			if (!userApprove.equals(process.getToUserId())) {
				process.setActive(false);
				hsFolderProcessRepo.save(process);
				addProcess(input.getId(), userApprove, null, HsFolderProcessEnum.PB_CHO_DUYET, null);
			}
		}
		return true;
	}

	public Boolean addShare(List<HsFolderShare> input) {
		if (input.isEmpty()) {
			return true;
		}
		Long folderId = input.get(0).getFolderId();
		HsFolder folder = findByFolderId(folderId);
		validatePermission(folder, FolderPermissionEnum.FULL, false);
		if (folder.getParentId() != null) {
			throw new RestExceptionHandler(Message.ACTION_FAILED);
		}

		List<Long> userIds = input.stream().map(i->i.getUserId()).distinct().collect(Collectors.toList());
		notifyService.addAll(userIds, folder.getId(), folder.getTitle(), DocumentTypeEnum.TU_HO_SO,
				NotificationHandleStatusEnum.HS_CV_CHIA_SE, ModuleCodeEnum.HSTL_CANHAN);
		return hsFolderShareService.add(input);
	}


	public Boolean transferFolder(Long folderId, Long parentId) {
		if (folderId.equals(parentId)) {
			throw new RestExceptionHandler("Thư mục không thể di chuyển vào chính nó");
		}
		HsFolder folder = findByFolderId(folderId);
		validatePermission(folder, FolderPermissionEnum.FULL, false);
		validatePermission(folder.getParentId(), FolderPermissionEnum.FULL, false);
		if (parentId == null || parentId.equals(0L)) {
			if (folder.getParentId() != null) {
				decreaseTotalItems(folder.getParentId(), 1);
				folder.setParentId(null);
			}
		} else {
			validatePermission(parentId, FolderPermissionEnum.FULL, false);
			HsFolder parent = hsFolderRepo.findByClientIdAndId(BussinessCommon.getClientId(), parentId);
			if (parent == null) {
				throw new RestExceptionHandler("Thư mục chứa không tồn tại, vui lòng kiểm tra lại!");
			}
			validatePermission(folderId, FolderPermissionEnum.FULL, false);
			if (!parentId.equals(folder.getParentId())) {
				increaseTotalItems(parentId, 1);
				if (folder.getParentId() != null) {
					decreaseTotalItems(folder.getParentId(), 1);
				}
				folder.setParentId(parentId);
			}
		}
		hsFolderRepo.save(folder);
		return true;
	}

	public void increaseTotalItems(Long folderId, long number) {
		hsFolderRepo.increaseTotalItems(folderId, number);
	}

	public void decreaseTotalItems(Long folderId, long number) {
		hsFolderRepo.decreaseTotalItems(folderId, number);
	}

	public FolderInfoDto getFolderDetailById(Long id) {
		FolderInfoDto result = new FolderInfoDto();
		result.setFolderDetail(hsFolderRepo.getFolderDetailById(id, BussinessCommon.getClientId()));
		result.setListTrack(hsFolderTrackingRepo.findByFolderId(id, BussinessCommon.getClientId()));
		return result;
	}

	public FolderDataDetailDto getDataDetailByFolderId(@Nullable Long folderId, FolderTypeEnum type) {
		HsFolderStatusEnum status = HsFolderStatusEnum.HS_TAI_LIEU;
		FolderDataDetailDto result = new FolderDataDetailDto();
		validatePermission1(folderId, null, false);
		Long clientId = BussinessCommon.getClientId();
		Long userId = BussinessCommon.getUserId();
		Long orgId = BussinessCommon.getOrgId();
		List<FolderDetailDto> listFolders;
		List<IconDetailDto> listDocuments;
		List<IconDetailDto> listFiles;
		String typeStr = null;
		if (type != null) {
			typeStr = type.name();
		}

		if (FolderTypeEnum.LUU_TRU_CA_NHAN.equals(type)) {
			status = null;
			typeStr = null;
		}

		if (folderId == null) {
			listFolders = hsFolderRepo.getListDetailByRoot(userId, orgId, FolderTypeEnum.CANHAN, FolderTypeEnum.COQUAN, status, clientId, true, typeStr).stream()
					.collect(collectingAndThen(toCollection(() -> new TreeSet<>(comparingLong(FolderDetailDto::getId))),
							ArrayList::new));
			listDocuments = hsFolderDocumentRepo.getListDetailByRoot(userId, DocumentTypeEnum.VAN_BAN_DEN, DocumentTypeEnum.VAN_BAN_DI, clientId, true);
			listFiles = hsFolderFileRepo.getListDetailByRoot(userId, clientId, true);
		} else {
			listFolders = hsFolderRepo.getListDetailByFolderId(userId, folderId, status, clientId, true, typeStr).stream()
					.collect(collectingAndThen(toCollection(() -> new TreeSet<>(comparingLong(FolderDetailDto::getId))),
							ArrayList::new));
			listDocuments = hsFolderDocumentRepo.getListDetailByFolderId(orgId, userId, folderId, DocumentTypeEnum.VAN_BAN_DEN, DocumentTypeEnum.VAN_BAN_DI, status, clientId, true);
			listFiles = hsFolderFileRepo.getListDetailByFolderId(folderId, status, clientId, true);
			btService.track(folderId, BusinessTracking.BusinessTrackingType.HO_SO);
		}
		result.setListFolder(listFolders);
		listDocuments.addAll(listFiles);
		result.setListIcon(listDocuments);
		return result;
	}

	/**
	 * Move file of a folder to other folder
	 * @param fileId id file
	 * @param folderIdFrom folder source
	 * @param folderIdTo folder destination
	 * @return True or False
	 */
	public Boolean transferFileToOtherFolder(Long fileId, Long folderIdFrom, Long folderIdTo, Long docId) {
		HsFolderFile fileHsFolderFile = new HsFolderFile();
		HsFolderDocument fileFolderDocument = new HsFolderDocument();

		if (folderIdFrom.equals(folderIdTo)) {
			throw new RestExceptionHandler("Thư mục không thể di chuyển vào chính nó");
		}

		fileHsFolderFile = findFolderFileByFileId(fileId);
		if(fileHsFolderFile == null) {
			if(hsFolderDocumentRepo.isDuplicateKeyById(folderIdTo, docId, DocumentTypeEnum.VAN_BAN_DEN) == 1) {
				throw new RestExceptionHandler(Message.FILE_DOCUMENT_EXISTS);
			} else {
				fileFolderDocument = findFolderDocumentByFileId(fileId);
			}
		}

		HsFolder folderTo = findByFolderId(folderIdTo);
		if (folderTo == null) {
			throw new RestExceptionHandler("Thư mục chứa không tồn tại, vui lòng kiểm tra lại!");
		}
		validatePermission(folderTo, FolderPermissionEnum.FULL, false);

		increaseTotalItems(folderIdTo, 1);
		decreaseTotalItems(folderIdFrom, 1);

		if(fileHsFolderFile != null) {
			fileHsFolderFile.setFolderId(folderIdTo);
			hsFolderFileRepo.save(fileHsFolderFile);
		} else {
			fileFolderDocument.setFolderId(folderIdTo);
			hsFolderDocumentRepo.save(fileFolderDocument);
		}

		return true;
	}

	/**
	 * Find file by id file
	 * @param fileId id file
	 * @return object HsFolderFile
	 */
	private HsFolderFile findFolderFileByFileId(Long fileId) {
		Optional<HsFolderFile> tmp = hsFolderFileRepo.findById(fileId);
		if (!tmp.isPresent()) {
			return null;
		}
		return tmp.get();
	}

	/**
	 * Find file by id file document
	 * @param fileId id file document
	 * @return object HsFolderDocument
	 */
	private HsFolderDocument findFolderDocumentByFileId(Long fileId) {
		Optional<HsFolderDocument> tmp = hsFolderDocumentRepo.findById(fileId);
		if (!tmp.isPresent()) {
			return null;
		}
		return tmp.get();
	}

	public FolderDetailDto getDetailByFolderId(@Nullable Long folderId, FolderTypeEnum type) {
		HsFolderStatusEnum status = HsFolderStatusEnum.HS_TAI_LIEU;
		String typeStr = null;
		if (type != null) {
			typeStr = type.name();
		}

		if (FolderTypeEnum.LUU_TRU_CA_NHAN.equals(type)) {
			status = null;
			typeStr = null;
		}

		Long clientId = BussinessCommon.getClientId();
		Long userId = BussinessCommon.getUserId();
		Long orgId = BussinessCommon.getOrgId();
		return hsFolderRepo.getDetailByFolderId(folderId, userId, orgId, FolderTypeEnum.CANHAN, FolderTypeEnum.COQUAN, status, clientId, true, typeStr);
	}

	public ListObjectDto<IconDetailDto> getListVBByFolderId(Long folderId, Pageable pageable) {
		HsFolderStatusEnum status = null;
		validatePermission1(folderId, null, true);
		Long clientId = BussinessCommon.getClientId();
		Page<IconDetailDto> listDocuments;
		List<IconDetailDto> listFiles;
		Long userId = BussinessCommon.getUserId();
		Long orgId = BussinessCommon.getOrgId();
		listDocuments = hsFolderDocumentRepo.getListDetailByFolderIdPagi(orgId, userId, folderId, DocumentTypeEnum.VAN_BAN_DEN,
				DocumentTypeEnum.VAN_BAN_DI, status, clientId, true, pageable);
		listFiles = hsFolderFileRepo.getListDetailByFolderId(folderId, status, clientId, true);
		List<IconDetailDto> newObject = new ArrayList<>();
		newObject.addAll(listDocuments.getContent().stream()
				.collect(collectingAndThen(toCollection(() -> new TreeSet<>(comparingLong(IconDetailDto::getId))),
						ArrayList::new)));
		newObject.addAll(listFiles);
		Page<IconDetailDto> page = new PageImpl<>(newObject, pageable,listDocuments.getTotalElements());
		return BussinessCommon.paging(page);
	}

	public List<FolderBasicDto> getListRootFolder(FolderTypeEnum type) {
		HsFolderStatusEnum status = HsFolderStatusEnum.HS_TAI_LIEU;
		Long clientId = BussinessCommon.getClientId();
		Long userId = BussinessCommon.getUserId();
		Long orgId = BussinessCommon.getOrgId();
		String typeStr = null;
		if(type != null) {
			typeStr = type.name();
		}

		if(FolderTypeEnum.LUU_TRU_CA_NHAN.equals(type)) {
			status = null;
			typeStr = null;
		}

		List<FolderBasicDto> result = hsFolderRepo.getListFolderByRootAndStatusAndActive(userId, orgId, FolderTypeEnum.CANHAN, FolderTypeEnum.COQUAN, status, clientId, typeStr);
		result.addAll(getListChildFolder(result, type));
		return result.stream()
                .collect(collectingAndThen(toCollection(() -> new TreeSet<>(comparingLong(FolderBasicDto::getId))),
                        ArrayList::new));
	}

	private List<FolderBasicDto> getListChildFolder(List<FolderBasicDto> listParent, FolderTypeEnum type) {
		List<FolderBasicDto> result = new ArrayList<>();
		if (listParent.isEmpty()) {
			return result;
		}
		HsFolderStatusEnum status = HsFolderStatusEnum.HS_TAI_LIEU;
		if (FolderTypeEnum.LUU_TRU_CA_NHAN.equals(type)) {
			status = null;
		}

		for (FolderBasicDto parent : listParent) {
			List<FolderBasicDto> child = hsFolderRepo.getChildrenByParentId(parent.getId(), status,
					BussinessCommon.getClientId(), true);
			result.addAll(child);
		}
		result.addAll(getListChildFolder(result, type));
		return result;
	}

	public Boolean deleteFolderById(Long id) {
		Long userId = BussinessCommon.getUserId();
		try {
			HsFolder folder = findByFolderId(id);
			if (userId.equals(folder.getCreateBy())) {
				folder.setActive(false);
				// decrease totalItems for parent folder
				hsFolderRepo.decreaseTotalItems(folder.getParentId(), 1L);
				return true;
			}

			validatePermission(folder, FolderPermissionEnum.FULL, true);
			HsFolderProcess oProcess = hsFolderProcessRepo.findFirstByFolderIdAndToUserIdAndClientIdAndActiveTrue(
					folder.getId(), userId, BussinessCommon.getClientId());
			if (oProcess != null) {
				oProcess.setActive(false);
				hsFolderProcessRepo.save(oProcess);
			}

			folder.setStatus(HsFolderStatusEnum.HS_TAI_LIEU);
			hsFolderRepo.save(folder);
		} catch (Exception e) {
			throw new RestExceptionHandler(Message.ACTION_FAILED);
		}
		return true;
	}

	private int countTotalPageNumberOfAllDocInFolder(Long folderId) {
		return hsFolderRepo.countTotalPageNumberOfAllDocInFolder(folderId, BussinessCommon.getClientId());
	}

	private HsFolder findByFolderId(Long folderId) {
		Optional<HsFolder> tmp = hsFolderRepo.findById(folderId);
		if (!tmp.isPresent()) {
			throw new RestExceptionHandler(Message.FOLDER_NOT_FOUND);
		}
		return tmp.get();
	}

	//Check quyền xem
	public boolean checkPermissionByDocId(Long docId, DocumentTypeEnum docType, Set<Long> docInChecked, Set<Long> docOutChecked, Set<Long> taskChecked, Set<Long> folderChecked, boolean isCheckDoc) {
		List<HsFolder> listFolder = hsFolderDocumentRepo.findFolderByDocIdAndTypeAndActive(docId, docType, true, BussinessCommon.getClientId());
		for (HsFolder folder : listFolder) {
			if (checkPermission(folder, null, true, docInChecked, docOutChecked, taskChecked, folderChecked, isCheckDoc)) {
				return true;
			}
		}
		return false;
	}
	//Check quyền thao tác
	public void validatePermission(Long folderId, FolderPermissionEnum permission, boolean byProcess) {
		if (folderId == null) {
			return;
		}
		HsFolder folder = findByFolderId(folderId);
		validatePermission(folder, permission, byProcess);
	}
	//Check quyền thao tác
	public void validatePermission(HsFolder folder, FolderPermissionEnum permission, boolean byProcess) {
		if (!checkPermission(folder, permission, byProcess)) {
			throw new RestExceptionHandler(Message.NO_PERMISSION);
		}
	}
	//Check quyền xem
	public void validatePermission1(Long folderId, FolderPermissionEnum permission, boolean byProcess) {
		if (folderId == null) {
			return;
		}
		HsFolder folder = findByFolderId(folderId);
		validatePermission1(folder, permission, byProcess);
	}
	//Check quyền xem
	public void validatePermission1(HsFolder folder, FolderPermissionEnum permission, boolean byProcess) {
		if (!checkPermission1(folder, permission, byProcess)) {
			throw new RestExceptionHandler(Message.NO_PERMISSION);
		}
	}
	//Check quyền thao tác
	private boolean checkPermission(HsFolder folder, FolderPermissionEnum permission, boolean byProcess) {
		Long userId = BussinessCommon.getUserId();
		Long orgId = BussinessCommon.getOrgId();
		Long clientId = BussinessCommon.getClientId();
		if (hsFolderRepo.checkPermission(folder.getId(), userId, orgId, permission, clientId, FolderTypeEnum.CANHAN, FolderTypeEnum.COQUAN)) {
			return true;
		}
		if (byProcess && hsFolderProcessRepo.checkPermissionByProcess(folder.getId(), userId, orgId, clientId)) {
			return true;
		}
		if (folder.getParentId() != null) {
			return checkPermission(folder.getParent(), permission, byProcess);
		}
		return false;
	}
	//Check quyền xem
	public boolean checkPermission(Long folderId, FolderPermissionEnum permission, boolean byProcess) {
		Set<Long> docInChecked = new HashSet<>();
		Set<Long> docOutChecked = new HashSet<>();
		Set<Long> taskChecked = new HashSet<>();
		Set<Long> folderChecked = new HashSet<>();
		Optional<HsFolder> folder = hsFolderRepo.findById(folderId);
		if (folder.isPresent()) {
			return checkPermission(folder.get(), permission, byProcess, docInChecked, docOutChecked, taskChecked, folderChecked);
		}
		return false;
	}
	//Check quyền xem
	public boolean checkPermission1(HsFolder folder, FolderPermissionEnum permission, boolean byProcess) {
		Set<Long> docInChecked = new HashSet<>();
		Set<Long> docOutChecked = new HashSet<>();
		Set<Long> taskChecked = new HashSet<>();
		Set<Long> folderChecked = new HashSet<>();
		return checkPermission(folder, permission, byProcess, docInChecked, docOutChecked, taskChecked, folderChecked);
	}


	private boolean checkPermission(HsFolder folder, FolderPermissionEnum permission, boolean byProcess,
			Set<Long> docInChecked, Set<Long> docOutChecked, Set<Long> taskChecked, Set<Long> folderChecked) {
		return checkPermission(folder, permission, byProcess, docInChecked, docOutChecked, taskChecked, folderChecked, true);
	}

	//Check quyền xem
	private boolean checkPermission(HsFolder folder, FolderPermissionEnum permission, boolean byProcess,
			Set<Long> docInChecked, Set<Long> docOutChecked, Set<Long> taskChecked, Set<Long> folderChecked, boolean isCheckDoc) {
		Long userId = BussinessCommon.getUserId();
		Long orgId = BussinessCommon.getOrgId();
		Long clientId = BussinessCommon.getClientId();
		if (!folderChecked.contains(folder.getId())) {
			folderChecked.add(folder.getId());
			if (hsFolderRepo.checkPermission(folder.getId(), userId, orgId, permission, clientId, FolderTypeEnum.CANHAN, FolderTypeEnum.COQUAN)) {
				return true;
			}
			if (byProcess && hsFolderProcessRepo.checkPermissionByProcess(folder.getId(), userId, orgId, clientId)) {
				return true;
			}
		}

		if (isCheckDoc) {
			List<IconBasicDto> result = hsFolderDocumentRepo.getListDocIdByFolderId(folder.getId(), DocumentTypeEnum.VAN_BAN_DEN, DocumentTypeEnum.VAN_BAN_DI, clientId, true);
			for (IconBasicDto t : result) {
				// Check văn bản đi
				if (DocumentTypeEnum.VAN_BAN_DI.equals(t.getDocType()) && docOutService.checkPermission(t.getId(),
						docInChecked, docOutChecked, taskChecked, folderChecked)) {
					return true;
				}
				// Check văn bản đến
				if (DocumentTypeEnum.VAN_BAN_DEN.equals(t.getDocType())
						&& docService.checkPermission(t.getId(), docInChecked, docOutChecked, taskChecked, folderChecked)) {
					return true;
				}
			}
		}
		if (folder.getParentId() != null) {
			return checkPermission(folder.getParent(), permission, byProcess, docInChecked, docOutChecked, taskChecked, folderChecked);
		}
		return false;
	}

	private HsFolderTracking addTracking(Long folderId, HsFolderTrackingEnum status, String comment) {
		return hsFolderTrackingRepo.save(new HsFolderTracking(folderId, status, comment));
	}

	private HsFolderProcess addProcess(Long folderId, Long toUserId, Long toOrgId, HsFolderProcessEnum status,
			String comment) {
		HsFolderProcess process = hsFolderProcessRepo.findFirstByFolderIdAndClientIdAndActiveTrue(folderId,
				BussinessCommon.getClientId());
		if (process == null) {
			process = new HsFolderProcess(folderId, BussinessCommon.getUserId(), toUserId, toOrgId, status, comment);
		} else {
			process.setToOrgId(toOrgId);
			process.setComment(comment);
			process.setStatus(status);
		}
		return hsFolderProcessRepo.save(process);
	}

	public Boolean transferToHSCV(Long folderId, Long userApprove, Long maintenance) {
		HsFolder folder = findByFolderId(folderId);
		if (folder.getParentId() != null) {
			throw new RestExceptionHandler("Thư mục con không được di chuyển sang HSCV");
		}

		if (folder.getTotalDoc() <= 0) {
			throw new RestExceptionHandler("Hồ sơ chưa có tài liệu không thế chuyển");
		}

		validatePermission(folder, FolderPermissionEnum.FULL, false);
		if (!HsFolderStatusEnum.HS_TAI_LIEU.equals(folder.getStatus())) {
			throw new RestExceptionHandler(Message.ACTION_FAILED);
		}
		if (folder.getParentId() != null) {
			throw new RestExceptionHandler(Message.ACTION_FAILED);
		}


		if(maintenance != null) {
			catService.valid(maintenance, Message.NOT_FOUND_OBJECT);
		}
		folder.setMaintenance(maintenance);
		transferToHSCV(folder, userApprove);
		return true;
	}

	private void transferToHSCV(HsFolder folder, Long userApprove) {
		folder.setStatus(HsFolderStatusEnum.HS_CONG_VIEC);
		hsFolderRepo.save(folder);
		addTracking(folder.getId(), HsFolderTrackingEnum.TRANSFER_PB, null);
		addProcess(folder.getId(), userApprove, null, HsFolderProcessEnum.PB_CHO_DUYET, null);

		if (!BussinessCommon.getUserId().equals(userApprove)) {
			notifyService.add(userApprove, folder.getId(), folder.getTitle(), DocumentTypeEnum.TU_HO_SO,
					NotificationHandleStatusEnum.HS_CV_CHO_DUYET, ModuleCodeEnum.HSTL_CONGVIEC);
		}
	}

	public Boolean finishHSCV(Long folderId) {
		HsFolder folder = findByFolderId(folderId);
		validatePermission(folder, FolderPermissionEnum.FULL, true);
		if (!(HsFolderStatusEnum.HS_CONG_VIEC.equals(folder.getStatus()) || HsFolderStatusEnum.HS_TRA_LAI.equals(folder.getStatus()))) {
			throw new RestExceptionHandler(Message.ACTION_FAILED);
		}
		List<HsFolderProcessEnum> status = Arrays.asList(HsFolderProcessEnum.PB_CHO_DUYET, HsFolderProcessEnum.PB_TRA_LAI, HsFolderProcessEnum.CQ_CHO_DUYET, HsFolderProcessEnum.CQ_TRA_LAI);
		HsFolderProcess process = hsFolderProcessRepo.findFirstByFolderIdAndStatusInAndActiveAndClientIdOrderByIdDesc(folderId, status, true, BussinessCommon.getClientId());
		if (process == null) {
			throw new RestExceptionHandler(Message.ACTION_FAILED);
		}
//		if (HsFolderProcessEnum.PB_TRA_LAI.equals(process.getStatus())) {
//			process.setStatus(HsFolderProcessEnum.PB_CHO_DUYET);
//		}

		// Cơ quan duyệt
		Long orgApprove = orgService.findParentIdByOrgId(folder.getOrgQLId());
		if (orgApprove == null) {
			orgApprove = folder.getOrgQLId();
		}

		addProcess(folderId, null, orgApprove, HsFolderProcessEnum.CQ_CHO_DUYET, null);
		addTracking(folderId, HsFolderTrackingEnum.FINISH, null);

		folder.setStatus(HsFolderStatusEnum.CQ_CHO_DUYET);
		hsFolderRepo.save(folder);

		//Delete all notification
		notifyService.deactiveAllByDocIdAndDocType(folder.getId(), DocumentTypeEnum.TU_HO_SO);

		//Add notification
		notifyService.add(process.getToUserId(), folder.getId(), folder.getTitle(), DocumentTypeEnum.TU_HO_SO,
				NotificationHandleStatusEnum.HS_DUYET_CQ, ModuleCodeEnum.HSTL_COQUAN);

		// for creator
		if (!BussinessCommon.getUserId().equals(folder.getCreateBy())) {
			notifyService.add(folder.getCreateBy(), folder.getId(), folder.getTitle(), DocumentTypeEnum.TU_HO_SO,
					NotificationHandleStatusEnum.HS_CV_DA_DUYET, ModuleCodeEnum.HSTL_CANHAN);
		}

		// Add notification
		List<Long> userIds = userService.findUserIdByOrgWithAuthority(orgApprove, AuthorityEnum.DUYET_HOSO);
		notifyService.addAll(userIds, folder.getId(), folder.getTitle(), DocumentTypeEnum.TU_HO_SO,
				NotificationHandleStatusEnum.HS_DUYET_CQ, ModuleCodeEnum.HSTL_COQUAN);

		return true;
	}

	public Boolean transferToHSCQ(Long folderId, Long orgApprove) {
		HsFolder folder = findByFolderId(folderId);
		if (orgApprove == null) {
			orgApprove = orgService.findParentIdByOrgId(folder.getOrgQLId());
			if (orgApprove == null) {
				orgApprove = folder.getOrgQLId();
			}
		}
		if (!(HsFolderStatusEnum.PB_DA_DUYET.equals(folder.getStatus())
				|| HsFolderStatusEnum.PB_TRA_LAI.equals(folder.getStatus()))) {
			throw new RestExceptionHandler(Message.ACTION_FAILED);
		}
		folder.setStatus(HsFolderStatusEnum.CQ_CHO_DUYET);
		addTracking(folderId, HsFolderTrackingEnum.TRANSFER_CQ, null);
		addProcess(folderId, null, orgApprove, HsFolderProcessEnum.CQ_CHO_DUYET, null);
		//Delete all notification
		notifyService.deactiveAllByDocIdAndDocType(folder.getId(), DocumentTypeEnum.TU_HO_SO);
		//Add notification
		List<Long> userIds = userService.findUserIdByOrgWithAuthority(orgApprove, AuthorityEnum.DUYET_HOSO);
		notifyService.addAll(userIds, folder.getId(), folder.getTitle(), DocumentTypeEnum.TU_HO_SO,
				NotificationHandleStatusEnum.HS_DUYET_CQ, ModuleCodeEnum.HSTL_COQUAN);
		return true;
	}

	public Boolean review(Long folderId, boolean approve, String comment, Long option, String organld) {
		Long userId = BussinessCommon.getUserId();
		Long orgId = BussinessCommon.getOrgId();
		Long clientId = BussinessCommon.getClientId();
		List<HsFolderProcessEnum> oldProcessStatus = null;
		HsFolderProcessEnum newProcessStatus = null;
		HsFolderStatusEnum oldFolderStatus = null;
		HsFolderStatusEnum newFolderStatus = null;
		HsFolderTrackingEnum trackStatus = null;
		HsFolderProcess process = null;
		NotificationHandleStatusEnum notiEnum = null;
		ModuleCodeEnum moduleEnum = null;
		if (option.equals(1L)) { //Duyệt hồ sơ phòng ban
			oldProcessStatus = Arrays.asList(HsFolderProcessEnum.PB_CHO_DUYET, HsFolderProcessEnum.PB_TRA_LAI);
			oldFolderStatus = HsFolderStatusEnum.PB_CHO_DUYET;
			moduleEnum = ModuleCodeEnum.HSTL_CONGVIEC;
			if (approve) {
				newProcessStatus = HsFolderProcessEnum.PB_DA_DUYET;
				newFolderStatus = HsFolderStatusEnum.PB_DA_DUYET;
				trackStatus = HsFolderTrackingEnum.DUYET_PB;
				notiEnum = NotificationHandleStatusEnum.HS_DONG_Y_PB;
			} else {
				newProcessStatus = HsFolderProcessEnum.PB_TRA_LAI;
				newFolderStatus = HsFolderStatusEnum.HS_TRA_LAI;
				trackStatus = HsFolderTrackingEnum.TRA_LAI_PB;
				notiEnum = NotificationHandleStatusEnum.HS_TRA_LAI_PB;
			}
			process = hsFolderProcessRepo.findFirstByFolderIdAndToUserIdAndStatusInAndActiveAndClientIdOrderByIdDesc(folderId, userId,
					oldProcessStatus, true, clientId);
		}
		if (option.equals(2L)) { //Duyệt hồ sơ cơ quan
			oldProcessStatus = Arrays.asList(HsFolderProcessEnum.CQ_CHO_DUYET, HsFolderProcessEnum.CQ_TRA_LAI);
			oldFolderStatus = HsFolderStatusEnum.CQ_CHO_DUYET;
			moduleEnum = ModuleCodeEnum.HSTL_PHONGBAN;
			if (approve) {
				newProcessStatus = HsFolderProcessEnum.CQ_DA_DUYET;
				newFolderStatus = HsFolderStatusEnum.CQ_DA_DUYET;
				trackStatus = HsFolderTrackingEnum.DUYET_CQ;
				notiEnum = NotificationHandleStatusEnum.HS_DONG_Y_CQ;
			} else {
				newProcessStatus = HsFolderProcessEnum.CQ_TRA_LAI;
				newFolderStatus = HsFolderStatusEnum.HS_TRA_LAI;
				trackStatus = HsFolderTrackingEnum.TRA_LAI_CQ;
				notiEnum = NotificationHandleStatusEnum.HS_TRA_LAI_CQ;
			}
			process = hsFolderProcessRepo.findFirstByFolderIdAndToOrgIdAndStatusInAndActiveAndClientIdOrderByIdDesc(folderId, orgId,
					oldProcessStatus, true, clientId);
		}
		//		HsFolder folder = hsFolderRepo.findByIdAndStatusAndClientId(folderId, oldFolderStatus, clientId);
		HsFolder folder = hsFolderRepo.findByClientIdAndId(clientId, folderId);
		if (folder == null) {
			throw new RestExceptionHandler(Message.FOLDER_NOT_FOUND);
		}
		if (process == null) {
			throw new RestExceptionHandler(Message.ACTION_FAILED);
		}

		//set font when approve
		setFontId(organld, folder);

		//Update process
		process.setStatus(newProcessStatus);
		process.setComment(comment);
		hsFolderProcessRepo.save(process);
		//Add tracking
		addTracking(folderId, trackStatus, comment);
		//Update folder status
		folder.setStatus(newFolderStatus);
		hsFolderRepo.save(folder);
		//Delete all notification
		notifyService.deactiveAllByDocIdAndDocType(folder.getId(), DocumentTypeEnum.TU_HO_SO);
		//Add notification
		notifyService.add(process.getFromUserId(), folder.getId(), folder.getTitle(), DocumentTypeEnum.TU_HO_SO, notiEnum, moduleEnum);
		return true;
	}

	private void setFontId(String organld, HsFolder folder) {
		if (BussinessCommon.convert(organld) == null) {
			return;
		}
		fontService.valid(organld, BussinessCommon.getOrgId());
		folder.setOrganld(organld);
	}

	@Getter
	private Long fromUserId = null;
	@Getter
	private Long toUserId = null;
	@Getter
	private Long toOrgId = null;
	@Getter
	private List<HsFolderStatusEnum> folderStatus = null;
	@Getter
	private List<HsFolderProcessEnum> processStatus = null;
	public void setCondition (Integer menu, Integer tab) {
		resetCondition();
		Long userId = BussinessCommon.getUserId();
		Long orgId = BussinessCommon.getOrgId();
		switch (menu) {
		case 1:
			toUserId = fromUserId = userId;
			if (tab.equals(1)) {
				folderStatus = Arrays.asList(HsFolderStatusEnum.HS_CONG_VIEC, HsFolderStatusEnum.HS_TRA_LAI);
				//processStatus = Arrays.asList(HsFolderProcessEnum.PB_CHO_DUYET);
			} else {
				folderStatus = Arrays.asList(HsFolderStatusEnum.PB_CHO_DUYET, HsFolderStatusEnum.CQ_CHO_DUYET);
				processStatus = Arrays.asList(HsFolderProcessEnum.PB_CHO_DUYET, HsFolderProcessEnum.PB_DA_DUYET, HsFolderProcessEnum.CQ_CHO_DUYET, HsFolderProcessEnum.CQ_DA_DUYET);
			}
			break;
		case 2:
			switch (tab) {
			case 1:
				toUserId = userId;
				folderStatus = Arrays.asList(HsFolderStatusEnum.PB_CHO_DUYET);
				processStatus = Arrays.asList(HsFolderProcessEnum.PB_CHO_DUYET);
				break;
			case 2:
				toUserId = userId;
				folderStatus = Arrays.asList(HsFolderStatusEnum.PB_DA_DUYET, HsFolderStatusEnum.PB_TRA_LAI);
				processStatus = Arrays.asList(HsFolderProcessEnum.PB_DA_DUYET);
				break;
			case 3:
				fromUserId = userId;
				//folderStatus = Arrays.asList(HsFolderStatusEnum.CQ_CHO_DUYET);
				processStatus = Arrays.asList(HsFolderProcessEnum.CQ_CHO_DUYET, HsFolderProcessEnum.CQ_DA_DUYET);
				break;
			default:
				toUserId = userId;
				folderStatus = Arrays.asList(HsFolderStatusEnum.HS_TRA_LAI);
				processStatus = Arrays.asList(HsFolderProcessEnum.PB_TRA_LAI);
				break;
			}
			break;
		case 3:
		default:
			toOrgId = orgId;
			switch (tab) {
			case 1:
				folderStatus = Arrays.asList(HsFolderStatusEnum.CQ_CHO_DUYET);
				processStatus = Arrays.asList(HsFolderProcessEnum.CQ_CHO_DUYET, HsFolderProcessEnum.CQ_TRA_LAI);
				break;
			case 2:
				folderStatus = Arrays.asList(HsFolderStatusEnum.CQ_DA_DUYET);
				processStatus = Arrays.asList(HsFolderProcessEnum.CQ_DA_DUYET);
				break;
			case 3:
				//				folderStatus = Arrays.asList(HsFolderStatusEnum.PB_TRA_LAI);
				//				processStatus = Arrays.asList(HsFolderProcessEnum.CQ_TRA_LAI);
				//				break;
			default:
				folderStatus = Arrays.asList(HsFolderStatusEnum.CQ_CHO_DUYET, HsFolderStatusEnum.HS_TRA_LAI);
				processStatus = Arrays.asList(HsFolderProcessEnum.CQ_CHO_DUYET, HsFolderProcessEnum.CQ_TRA_LAI);
				break;
			}
		}
	}

	private void resetCondition() {
		this.fromUserId = null;
		this.toUserId = null;
		this.toOrgId = null;
		this.folderStatus = null;
		this.processStatus = null;
	}

	public Page<FolderDetailDto> getListHoso(Long orgQLId, Long createBy, String folderName, Integer monthCreate,
			Integer yearCreate, String maHoso, Long thoiHanId, Integer tab, Integer menu, FolderTypeEnum type,
			Pageable pageable) {
		Long clientId = BussinessCommon.getClientId();
		setCondition(menu, tab);
		if (menu.equals(1)) {
			return hsFolderRepo.getListHoSo2(orgQLId, createBy, folderName, monthCreate, yearCreate, maHoso, thoiHanId,
					fromUserId, toUserId, toOrgId, folderStatus, processStatus, clientId, pageable);
		}

		return hsFolderRepo.getListHoSo(orgQLId, createBy, folderName, monthCreate, yearCreate, maHoso, thoiHanId,
				fromUserId, toUserId, toOrgId, folderStatus, processStatus, clientId, pageable);
	}

	public List<HsFolder> getListApprove(Long orgId) {
		List<HsFolderStatusEnum> folderStatus = Arrays.asList(HsFolderStatusEnum.PB_DA_DUYET, HsFolderStatusEnum.CQ_DA_DUYET);
		List<Long> orgIds = orgService.orgAndSub(orgId);
		return hsFolderRepo.findByStatusInAndOrgQLIdInAndClientIdAndActiveTrue(folderStatus, orgIds, BussinessCommon.getClientId());
	}

	// get data corresponding branch root
	public List<FolderTree> treeToList(List<HsFolder> folders, boolean checkHeadings) {
		List<FolderTree> rs = new ArrayList<>();
		List<FolderTree> childs;
		List<FolderTree> tree = buildTree(folders);
		if (checkHeadings) {
			setArticleFolder(tree, checkHeadings);
		}
		for (FolderTree i : tree) {
			childs = i.getChildren();
			i.setChildren(null);
			rs.add(i);
			rs.addAll(level(childs, new ArrayList<>()));
		}
		return rs;
	}

	public List<FolderTree> level(List<FolderTree> headings, List<FolderTree> rs) {
		if (headings.isEmpty()) {
			return rs;
		}

		List<FolderTree> childs = new ArrayList<>();
		for (FolderTree i : headings) {
			if (i.getParent() != null) {
				i.setHeadingsId(i.getParent().getHeadingsId());
			}
			childs = i.getChildren();
			if (!rs.contains(i)) {
				i.setChildren(null);
				rs.add(i);
			}

			level(childs, rs);
		}
		return rs;
	}

	public ReportMenuDto getReportMenu() {
		User u = BussinessCommon.getUser();
		List<ReportProcessDto> pList = hsFolderRepo.findByUserId(u.getId(), u.getOrg(), u.getClientId());
		return new ReportMenuDto(pList);
	}

	public List<HsFolder> approved() {
		User u = BussinessCommon.getUser();
		Long userId = u.getId();
		Long clientId = u.getClientId();
		return hsFolderRepo.approved(HsFolderStatusEnum.CQ_DA_DUYET, userId, clientId);
	}

	public CriteriaSearchDto getCriteriaSearch() {
		Long clientId = BussinessCommon.getClientId();
		List<Integer> yearFolders = hsFolderRepo.getYearFolders(clientId);
		List<String> typeFolders = hsFolderRepo.getTypeFolders(clientId);
		List<LabelValueId<String>> maintenances = catService.findByCategoryCode(Constant.THHS);
		maintenances.add(new LabelValueId<>(0L, "", "Bảo quản có thời hạn"));
		return new CriteriaSearchDto(yearFolders, typeFolders, maintenances);
	}

	public List<HsFolder> getListApprove(String text, Integer yearFolders, String typeFolders, Long maintenance,
			Date from, Date to, Long orgId) {
		List<HsFolder> rs = new ArrayList<>();
		for (HsFolder i : getListApprove(orgId)) {
			Category mObj = i.getMaintenanceObj(); // thời hạn bảo quản
			if ((text == null || i.getTitle().toLowerCase().contains(text)
					|| (i.getFileCode()!= null && i.getFileCode().toLowerCase().contains(text))
					|| (i.getFileCode()!= null && (i.getTitle().toLowerCase() + " - " + i.getFileCode().toLowerCase()).contains(text))
					|| (i.getHeadingsObj() != null && i.getHeadingsObj().getName().toLowerCase().contains(text)))
					&& (yearFolders == null || i.getYear() == null || yearFolders.equals(i.getYear()))
					&& (typeFolders == null || typeFolders.equalsIgnoreCase(i.getFileCode()))
					&& (maintenance == null
					|| (maintenance.equals(0l) && mObj != null && !Boolean.TRUE.equals(mObj.getIsDefault()))
					|| maintenance.equals(i.getMaintenance()))
					&& (from == null || (i.getUpdateDate() != null && i.getUpdateDate().compareTo(from) >= 0))
					&& (to == null || (i.getUpdateDate() != null && i.getUpdateDate().compareTo(to) <= 0))
					) {
				rs.add(i);
			}
		}
		return rs;
	}

	public List<FolderTree> buildTree(List<HsFolder> folders) {
		Map<Long, List<FolderTree>> pMap = new HashMap<>();
		Map<Long, FolderTree> kv = new HashMap<>();
		FolderTree i;
		for (HsFolder f : folders) {
			i = new FolderTree(f);
			if (!pMap.containsKey(i.getId())) {
				pMap.put(i.getId(), new ArrayList<>());
			}

			if (!kv.containsKey(i.getId())) {
				kv.put(i.getId(), i);
			}

			setParentChildsMap(null, i, pMap, kv);// root
		}

		return buildTree(pMap, kv);
	}

	private List<FolderTree> buildTree(Map<Long, List<FolderTree>> pMap, Map<Long, FolderTree> kv) {
		List<FolderTree> rs = new ArrayList<>();
		Map<FolderTree, List<FolderTree>> fMap = new HashMap<>();

		pMap.forEach((k, v) -> {
			FolderTree value = null;
			if(kv.containsKey(k)) {
				value = kv.get(k);
			}

			if(value != null && value.getParentId() == null) {
				rs.add(value);
			}

			if(value != null) {
				fMap.put(value, v);
			}
		});

		setChilds(rs, fMap);
		return rs;
	}

	private void setChilds(List<FolderTree> parents, Map<FolderTree, List<FolderTree>> pMap) {
		if (parents.isEmpty()) {
			return;
		}
		for (FolderTree p : parents) {
			if (pMap.containsKey(p)) {
				p.setChildren(pMap.get(p));
				setChilds(p.getChildren(), pMap);
			}
		}
	}

	public void setParentChildsMap(FolderTree child, FolderTree cursor, Map<Long, List<FolderTree>> pChildsMap, Map<Long, FolderTree> kvMap) {
		if(!kvMap.containsKey(cursor.getId())) {
			kvMap.put(cursor.getId(), cursor);
		}

		FolderTree tmp = getPFolder(cursor);
		if (tmp == null) {
			push(cursor.getId(), child, pChildsMap);
			return;
		}

		push(tmp.getId(), cursor, pChildsMap);
		setParentChildsMap(cursor, tmp, pChildsMap, kvMap);
	}

	private void push(Long key, FolderTree cursor, Map<Long, List<FolderTree>> pMap) {
		List<FolderTree> value;
		if (pMap.containsKey(key)) {
			value = pMap.get(key);
		} else {
			value = new ArrayList<>();
		}

		if (cursor != null && !value.contains(cursor)) {
			value.add(cursor);
		}

		pMap.put(key, value);
	}

	public FolderTree getPFolder(FolderTree f) {
		if (f.getParentId() == null) {
			return null;
		}

		if (f.getParent() == null) {
			return cast(f.getParentId());
		}

		return f.getParent();
	}

	private FolderTree cast(Long id) {
		HsFolder f = valid(id, Message.NOT_FOUND_OBJECT);
		return new FolderTree(f);
	}

	/**
	 * Set article for folders tree
	 * @param tree
	 */
	private void setArticleFolder(List<FolderTree> tree, boolean checkHeadings) {
		Map<Long, Integer> map = new HashMap<>();
		int count = 0;
		for (FolderTree i : tree) {
			Long key = i.getHeadingsId();
			if (key == null) {
				continue;
			}

			if (checkHeadings) {
				if (map.containsKey(key)) {
					count = map.get(key) + 1;
				} else {
					count = 1;
				}
				map.put(key, count);
			} else {
				count++;
			}
			i.setArticle(count + "");
			i.setTitle(i.getArticle() + HeadingsService.BREAK + " " + i.getTitle());
			setArticleFolderChild(i, new HashMap<>());
		}
	}

	private void setArticleFolderChild(FolderTree parent, Map<Long, Integer> map) {
		List<FolderTree> childrens = parent.getChildren();
		if (childrens.isEmpty()) {
			return;
		}
		Long key = parent.getId();
		int count = 1;
		for (FolderTree i : childrens) {
			if (map.containsKey(key)) {
				count = map.get(key) + 1;
			}
			map.put(key, count);
			i.setArticle(parent.getArticle() + HeadingsService.BREAK + count);
			i.setTitle(i.getArticle() + HeadingsService.BREAK + " " + i.getTitle());
			setArticleFolderChild(i, map);
		}
	}

	public Boolean stopShare(Long folderId) {
		HsFolder folder = valid(folderId, Message.NOT_FOUND_OBJECT);
		try {
			hsFolderShareService.inActive(folder);
			return true;
		} catch (Exception e) {
			log.error("Something wrong...", e);
			return false;
		}
	}

	public List<HsFolder> getByIds(List<Long> ids) {
		return hsFolderRepo.getByIds(ids, BussinessCommon.getClientId());
	}

	public Page<HsFolderRecordListDto> listApprove(Integer page) {
		Pageable pageable = BussinessCommon.castToPageable(page);
		return hsFolderRepo.listApprove(BussinessCommon.getClientId(), pageable);
	}
}
