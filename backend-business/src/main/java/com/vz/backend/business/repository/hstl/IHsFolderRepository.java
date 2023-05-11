package com.vz.backend.business.repository.hstl;

import java.util.List;

import javax.transaction.Transactional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.config.FolderPermissionEnum;
import com.vz.backend.business.config.FolderTypeEnum;
import com.vz.backend.business.config.HsFolderProcessEnum;
import com.vz.backend.business.config.HsFolderStatusEnum;
import com.vz.backend.business.domain.hstl.HsFolder;
import com.vz.backend.business.dto.hstl.FolderBasicDto;
import com.vz.backend.business.dto.hstl.FolderDetailDto;
import com.vz.backend.business.dto.hstl.ReportProcessDto;
import com.vz.backend.business.dto.hstl.ecm.HsFolderRecordListDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IHsFolderRepository extends IRepository<HsFolder> {

	@Query("SELECT count(*) > 0 FROM HsFolderShare fs RIGHT JOIN HsFolder f ON f.id = fs.folderId AND fs.active is true "
			+ "WHERE f.id = :folderId AND f.clientId = :clientId "
			+ "AND (f.createBy=:userId OR ("
			+ "    ( (f.folderType = :canhan AND fs.userId = :userId) "
			+ "      OR  (f.folderType = :coquan AND f.orgQLId = :orgId)) "
			+ "      AND (:permission is null OR fs.permission = :permission) )"
			+ ")")
	boolean checkPermission(Long folderId, Long userId, Long orgId, FolderPermissionEnum permission, Long clientId,
			FolderTypeEnum canhan, FolderTypeEnum coquan);

	//
	@Query("SELECT new com.vz.backend.business.dto.hstl.FolderDetailDto(f, u.fullName, fs.userId) "
			+ "FROM HsFolder f "
			+ "LEFT JOIN HsFolderShare fs ON fs.folderId = f.id AND fs.active = :active AND fs.clientId = :clientId "
			+ "INNER JOIN User u ON f.createBy = u.id "
			+ "WHERE f.id = :folderId "
			+ "AND ("
			+ "    (f.folderType = :canhan AND (f.createBy=:userId OR fs.userId = :userId)) OR (f.folderType = :coquan AND f.orgQLId = :orgId)) "
			+ "AND (:status IS NULL OR f.status = :status) " + "AND (:type IS NOT NULL OR f.createBy = :userId) "
			+ "AND (f.clientId = :clientId AND f.active = :active ) "
			+ "AND (:type IS NULL OR :type = CASE WHEN f.maintenanceObj.code = 'THVV' THEN 'LUU_TRU_VINH_VIEN' WHEN f.maintenanceObj.code != 'THVV' THEN 'LUU_TRU_CO_QUAN' END )")
	FolderDetailDto getDetailByFolderId(Long folderId, Long userId, Long orgId, FolderTypeEnum canhan, FolderTypeEnum coquan,
			HsFolderStatusEnum status, Long clientId, Boolean active, String type);
	
	@Query("SELECT new com.vz.backend.business.dto.hstl.FolderDetailDto(f, u.fullName, fs.userId) "
			+ "FROM HsFolder f "
			+ "LEFT JOIN HsFolderShare fs ON fs.folderId = f.id AND fs.active = :active AND fs.clientId = :clientId "
			+ "INNER JOIN User u ON f.createBy = u.id "
			+ "WHERE f.parentId is null AND ("
			+ "    (f.folderType = :canhan AND (f.createBy=:userId OR fs.userId = :userId)) OR (f.folderType = :coquan AND f.orgQLId = :orgId)) "
			+ "AND (:status IS NULL OR f.status = :status) " + "AND (:type IS NOT NULL OR f.createBy = :userId) "
			+ "AND (f.clientId = :clientId AND f.active = :active ) "
			+ "AND (:type IS NULL OR :type = CASE WHEN f.maintenanceObj.code = 'THVV' THEN 'LUU_TRU_VINH_VIEN' WHEN f.maintenanceObj.code != 'THVV' THEN 'LUU_TRU_CO_QUAN' END )")
	List<FolderDetailDto> getListDetailByRoot(Long userId, Long orgId, FolderTypeEnum canhan, FolderTypeEnum coquan,
			HsFolderStatusEnum status, Long clientId, Boolean active, String type);

	@Query("SELECT new com.vz.backend.business.dto.hstl.FolderDetailDto(f, u.fullName, fs.userId) "
			+ "FROM HsFolder f "
			+ "LEFT JOIN HsFolderShare fs ON fs.folderId = f.id AND fs.active = :active AND fs.clientId = :clientId "
			+ "INNER JOIN User u ON f.createBy = u.id " + "WHERE f.parentId = :folderId "
			+ "AND (:status IS NULL OR f.status = :status) " + "AND (:type IS NOT NULL OR f.createBy = :userId) "
			+ "AND f.clientId = :clientId AND (:active is null OR f.active = :active) "
			+ "AND (:type IS NULL OR :type = CASE WHEN f.maintenanceObj.code = 'THVV' THEN 'LUU_TRU_VINH_VIEN' WHEN f.maintenanceObj.code != 'THVV' THEN 'LUU_TRU_CO_QUAN' END) ")
	List<FolderDetailDto> getListDetailByFolderId(Long userId, Long folderId, HsFolderStatusEnum status, Long clientId,
			Boolean active, String type);

	@Query("SELECT new com.vz.backend.business.dto.hstl.FolderBasicDto(f, fs.userId) "
			+ "FROM HsFolder f LEFT JOIN HsFolderShare fs ON fs.folderId = f.id AND fs.active=TRUE AND fs.clientId=:clientId "
			+ "WHERE ((f.folderType = :canhan AND (f.createBy=:userId OR fs.userId = :userId)) "
			+ "OR (f.folderType = :coquan AND f.orgQLId = :orgId)) "
			+ "AND (:status IS NULL OR f.status = :status) AND f.parentId is null "
			+ "AND f.clientId = :clientId AND f.active = TRUE "
			+ "AND (:type IS NULL OR :type = CASE WHEN f.maintenanceObj.code = 'THVV' THEN 'LUU_TRU_VINH_VIEN' WHEN f.maintenanceObj.code != 'THVV' THEN 'LUU_TRU_CO_QUAN' END )")
	List<FolderBasicDto> getListFolderByRootAndStatusAndActive(Long userId, Long orgId, FolderTypeEnum canhan,
			FolderTypeEnum coquan, HsFolderStatusEnum status, Long clientId, String type);

	@Query("SELECT new com.vz.backend.business.dto.hstl.FolderBasicDto(f, fs.userId) "
			+ "FROM HsFolder f " 
			+ "LEFT JOIN HsFolderShare fs ON fs.folderId = f.id AND fs.active = :active AND fs.clientId = :clientId "
			+ "WHERE (:status IS NULL OR f.status = :status) AND f.parentId = :parentId "
			+ "AND f.clientId = :clientId AND (:active is null OR f.active = :active)")
	List<FolderBasicDto> getChildrenByParentId(Long parentId, HsFolderStatusEnum status, Long clientId, Boolean active);

	@Query("SELECT new com.vz.backend.business.dto.hstl.FolderBasicDto(f.id, f.parentId, f.title, f.fileCode) "
			+ "FROM HsFolder f " + "WHERE f.parentId = :folderId "
			+ "AND f.clientId = :clientId AND (:active is null OR f.active = :active)")
	List<FolderBasicDto> getListFolderByFolderIdAndActive(Long folderId, Long clientId, Boolean active);

	@Modifying
	@Transactional
	@Query("UPDATE HsFolder f SET f.totalDoc = f.totalDoc + :number " + "WHERE :folderId != null AND f.id = :folderId")
	void increaseTotalItems(Long folderId, long number);

	@Modifying
	@Transactional
	@Query("UPDATE HsFolder f SET f.totalDoc = f.totalDoc - :number " + "WHERE :folderId != null AND f.id = :folderId")
	void decreaseTotalItems(Long folderId, long number);

	HsFolder findByIdAndStatusAndClientId(Long folderId, HsFolderStatusEnum status, Long clientId);

	@Query("SELECT new com.vz.backend.business.dto.hstl.FolderDetailDto(f, u1.fullName, u2.id, u2.fullName, f.maintenanceObj.name, org.name) "
			+ "FROM HsFolder f INNER JOIN HsFolderProcess p ON p.folderId = f.id AND p.active is true "
			+ "INNER JOIN User u1 ON u1.id = f.createBy " + "LEFT JOIN User u2 ON u2.id = p.toUserId "
			+ "INNER JOIN Organization org ON org.id = f.orgQLId "
			+ "WHERE ((:folderStatus) is null OR f.status IN (:folderStatus)) "
			+ "AND ((:pStatus) is null OR p.status IN (:pStatus)) "
			+ "AND (:fromUserId is null OR p.fromUserId = :fromUserId) "
			+ "AND (:toUserId is null OR p.toUserId = :toUserId) " + "AND (:toOrgId is null OR p.toOrgId = :toOrgId) "
			+ "AND (:orgQLId is null OR f.orgQLId = :orgQLId) " + "AND (:createBy is null OR f.createBy = :createBy) "
			+ "AND (:folderName is null OR lower(f.title) like %:folderName%) "
			+ "AND (:monthCreate is null OR month(f.createDate) = :monthCreate) "
			+ "AND (:yearCreate is null OR year(f.createDate) = :yearCreate) "
			+ "AND (:maHoso is null OR lower(f.fileCode) like %:maHoso%) "
			+ "AND (:thoiHanId is null OR f.maintenance = :thoiHanId) "
			+ "AND f.clientId = :clientId AND f.active = TRUE "
			//			+ "AND :type = CASE WHEN f.maintenanceObj.code = 'THVV' THEN 'LUU_TRU_VINH_VIEN' WHEN f.maintenanceObj.code != 'THVV' THEN 'LUU_TRU_CO_QUAN' END "
			)
	Page<FolderDetailDto> getListHoSo(Long orgQLId, Long createBy, String folderName, Integer monthCreate,
			Integer yearCreate, String maHoso, Long thoiHanId, Long fromUserId, Long toUserId, Long toOrgId,
			List<HsFolderStatusEnum> folderStatus, List<HsFolderProcessEnum> pStatus, Long clientId,
			//			String type,
			Pageable pageable);

	@Query("SELECT new com.vz.backend.business.dto.hstl.FolderDetailDto(f, u1.fullName, u2.id, u2.fullName, f.maintenanceObj.name, org.name) "
			+ "FROM HsFolder f INNER JOIN HsFolderProcess p ON p.folderId = f.id AND p.active is true "
			+ "INNER JOIN User u1 ON u1.id = f.createBy " + "LEFT JOIN User u2 ON u2.id = p.toUserId "
			+ "INNER JOIN Organization org ON org.id = f.orgQLId "
			+ "WHERE ((:folderStatus) is null OR f.status IN (:folderStatus)) "
			+ "AND ((:pStatus) is null OR p.status IN (:pStatus)) "
			+ "AND ((:fromUserId is null OR p.fromUserId = :fromUserId) "
			+ "OR (:toUserId is null OR p.toUserId = :toUserId)) " + "AND (:toOrgId is null OR p.toOrgId = :toOrgId) "
			+ "AND (:orgQLId is null OR f.orgQLId = :orgQLId) " + "AND (:createBy is null OR f.createBy = :createBy) "
			+ "AND (:folderName is null OR lower(f.title) like %:folderName%) "
			+ "AND (:monthCreate is null OR month(f.createDate) = :monthCreate) "
			+ "AND (:yearCreate is null OR year(f.createDate) = :yearCreate) "
			+ "AND (:maHoso is null OR lower(f.fileCode) like %:maHoso%) "
			+ "AND (:thoiHanId is null OR f.maintenance = :thoiHanId) "
			+ "AND f.clientId = :clientId AND f.active = TRUE "
			//			+ "AND :type = CASE WHEN f.maintenanceObj.code = 'THVV' THEN 'LUU_TRU_VINH_VIEN' WHEN f.maintenanceObj.code != 'THVV' THEN 'LUU_TRU_CO_QUAN' END "
			)
	Page<FolderDetailDto> getListHoSo2(Long orgQLId, Long createBy, String folderName, Integer monthCreate,
			Integer yearCreate, String maHoso, Long thoiHanId, Long fromUserId, Long toUserId, Long toOrgId,
			List<HsFolderStatusEnum> folderStatus, List<HsFolderProcessEnum> pStatus, Long clientId,
			//			String type,
			Pageable pageable);

	@Query("SELECT new com.vz.backend.business.dto.hstl.FolderDetailDto(f.id, f.title, f.fileCode, f.totalDoc, f.folderType, f.createBy, f.maintenance, f.createDate, u.fullName) "
			+ "FROM HsFolder f INNER JOIN User u ON f.createBy = u.id "
			+ "WHERE f.id = :id AND f.clientId = :clientId ")
	FolderDetailDto getFolderDetailById(Long id, Long clientId);

	@Query("SELECT f FROM HsFolder f WHERE f.active=TRUE AND f.clientId = :clientId AND ((f.title = :title AND f.parentId =:parentId) OR f.fileCode = :fileCode OR f.fileNotation =:fileNotation)")
	List<HsFolder> getByInfo(String title, Long parentId, String fileCode, String fileNotation, Long clientId);

	List<HsFolder> findByStatusInAndOrgQLIdInAndClientIdAndActiveTrue(List<HsFolderStatusEnum> folderStatus,
			List<Long> orgIds, Long clientId);

	@Query(name = "HsFolder.menu", nativeQuery = true)
	List<ReportProcessDto> findByUserId(Long userId, Long orgId, Long clientId);

	@Query("SELECT " + "f" + " FROM HsFolder f " + "INNER JOIN User u on u.id=:userId "
			+ "WHERE f.clientId=:clientId AND f.orgQLId IN (u.org, u.orgModel.parentId) " + "AND f.status=:cqDaDuyet")
	List<HsFolder> approved(HsFolderStatusEnum cqDaDuyet, Long userId, Long clientId);

	@Query("SELECT DISTINCT(f.year) FROM HsFolder f WHERE f.clientId=:clientId AND f.active=TRUE AND f.year IS NOT NULL ORDER BY f.year")
	List<Integer> getYearFolders(Long clientId);

	@Query("SELECT DISTINCT(f.fileNotation) FROM HsFolder f WHERE f.clientId=:clientId AND f.active=TRUE AND f.fileNotation IS NOT NULL ORDER BY f.fileNotation")
	List<String> getTypeFolders(Long clientId);

	@Query("SELECT CASE WHEN (SUM(l.pageAmount) + SUM(d.pageAmount)) IS NULL THEN 0 ELSE (SUM(l.pageAmount) + SUM(d.pageAmount)) END FROM HsFolder f "
			+ " LEFT JOIN HsFolderDocument d ON f.id = d.folderId AND f.clientId = d.clientId AND f.active = d.active"
			+ " LEFT JOIN HsFolderFile l ON f.id = l.folderId AND f.clientId = l.clientId AND f.active = l.active"
			+ " WHERE f.id = :folderId AND f.clientId = :clientId AND f.active =TRUE")
	int countTotalPageNumberOfAllDocInFolder(Long folderId, Long clientId);

	@Query("SELECT f FROM HsFolder f WHERE f.clientId=:clientId AND f.id IN (:ids) AND f.active=TRUE")
	List<HsFolder> getByIds(List<Long> ids, Long clientId);

	@Query("SELECT NEW com.vz.backend.business.dto.hstl.ecm.HsFolderRecordListDto(f, false) FROM HsFolder f "
			+ " WHERE f.clientId=:clientId AND f.active=TRUE AND f.status = 'CQ_DA_DUYET'"
			+ " AND f.id NOT IN (SELECT fr.hsFolderId FROM HsFolderRecord fr WHERE fr.status > 1 AND fr.clientId = :clientId)"
			+ " ORDER BY f.createDate DESC")
	Page<HsFolderRecordListDto> listApprove(Long clientId, Pageable page);
}
