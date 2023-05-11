package com.vz.backend.business.repository.hstl;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.config.HsFolderStatusEnum;
import com.vz.backend.business.domain.hstl.HsFolder;
import com.vz.backend.business.domain.hstl.HsFolderDocument;
import com.vz.backend.business.dto.hstl.DocumentReportDto;
import com.vz.backend.business.dto.hstl.FolderDetailBasicDto;
import com.vz.backend.business.dto.hstl.FolderDocumentDto;
import com.vz.backend.business.dto.hstl.IconBasicDto;
import com.vz.backend.business.dto.hstl.IconDetailDto;
import com.vz.backend.business.dto.hstl.export.ContentDoc;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IHsFolderDocumentRepository extends IRepository<HsFolderDocument>{

	@Query("SELECT distinct new com.vz.backend.business.dto.hstl.IconDetailDto(fd.id, fd.docId, d.preview, d1.preview, d.numberArrivalStr, d1.numberOrSign, fd.createDate, fd.type) "
			+ "FROM HsFolderDocument fd "
			+ "LEFT JOIN HsFolderProcess p ON fd.folderId = p.folderId AND p.active is true "
			+ "LEFT JOIN Documents d ON fd.type = :vanBanDen AND d.id = fd.docId AND d.active = :active "
			+ "LEFT JOIN DocumentOut d1 ON fd.type = :vanBanDi AND d1.id = fd.docId AND d1.active = :active "
			+ "WHERE fd.folderId is null  AND (fd.createBy = :userId OR p.toUserId =:userId OR p.fromUserId =:userId) "
			//			+ "OR (fd.createBy != :userId AND fs.userId = :userId)) "
			+ "AND fd.clientId = :clientId AND (:active is null OR fd.active = :active)")
	List<IconDetailDto> getListDetailByRoot(Long userId, DocumentTypeEnum vanBanDen, DocumentTypeEnum vanBanDi, Long clientId, Boolean active);

	@Query("SELECT distinct new com.vz.backend.business.dto.hstl.IconDetailDto(fd.id, fd.docId, d.preview, d1.preview, d.numberArrivalStr, d1.numberOrSign, fd.createDate, fd.type) "
			+ "FROM HsFolderDocument fd "
			+ "INNER JOIN HsFolder hs ON hs.id = fd.folderId AND hs.active is true "
//			+ "LEFT JOIN HsFolderProcess p ON fd.folderId = p.folderId AND p.active is true "
			+ "LEFT JOIN HsFolderShare fs ON fs.folderId = fd.folderId AND fs.active = TRUE "
			+ "LEFT JOIN Documents d ON fd.type = :vanBanDen AND d.id = fd.docId AND d.active = :active "
			+ "LEFT JOIN DocumentOut d1 ON fd.type = :vanBanDi AND d1.id = fd.docId AND d1.active = :active "
			+ "WHERE fd.folderId = :folderId AND (:status is null OR fd.folder.status = :status) "
//			+ "AND (fd.createBy = :userId OR fs.userId =:userId OR p.toUserId =:userId OR p.fromUserId =:userId) "
			+ "AND (fd.createBy = :userId OR fs.userId =:userId "
			+ "  OR fd.folderId IN (SELECT p.folderId FROM HsFolderProcess p WHERE (p.toUserId =:userId OR p.fromUserId =:userId OR p.toOrgId=:orgId) AND p.clientId=:clientId AND p.active=TRUE)) "
			+ "AND fd.clientId = :clientId AND (:active is null OR fd.active = :active)")
	List<IconDetailDto> getListDetailByFolderId(Long orgId, Long userId, Long folderId, DocumentTypeEnum vanBanDen, DocumentTypeEnum vanBanDi, HsFolderStatusEnum status, Long clientId, Boolean active);

	@Query("SELECT new com.vz.backend.business.dto.hstl.IconDetailDto(fd.id, fd.docId, d.preview, d1.preview, d.numberArrivalStr, d1.numberOrSign, d.createDate, fd.type) "
			+ "FROM HsFolderDocument fd "
			+ "INNER JOIN HsFolder hs ON hs.id = fd.folderId AND hs.active is true "
//			+ "LEFT JOIN HsFolderProcess p ON fd.folderId = p.folderId AND p.active is true "
			+ "LEFT JOIN HsFolderShare fs ON fs.folderId = fd.folderId AND fs.active = TRUE "
			+ "LEFT JOIN Documents d ON fd.type = :vanBanDen AND d.id = fd.docId AND d.active = :active "
			+ "LEFT JOIN DocumentOut d1 ON fd.type = :vanBanDi AND d1.id = fd.docId AND d1.active = :active "
			+ "WHERE fd.folderId = :folderId AND (:status is null OR fd.folder.status = :status) "
//			+ "AND (fd.createBy = :userId OR fs.userId =:userId OR p.toUserId =:userId OR p.fromUserId =:userId) "
			+ "AND (fd.createBy = :userId OR fs.userId =:userId "
			+ "  OR fd.folderId IN (SELECT p.folderId FROM HsFolderProcess p WHERE (p.toUserId =:userId OR p.fromUserId =:userId OR p.toOrgId=:orgId) AND p.clientId=:clientId AND p.active=TRUE)) "
			+ "AND fd.clientId = :clientId AND (:active is null OR fd.active = :active)")
	Page<IconDetailDto> getListDetailByFolderIdPagi(Long orgId, Long userId, Long folderId, DocumentTypeEnum vanBanDen, DocumentTypeEnum vanBanDi, HsFolderStatusEnum status, Long clientId, Boolean active, Pageable pageable);


	@Query("SELECT new com.vz.backend.business.dto.hstl.FolderDocumentDto(fd) FROM HsFolderDocument fd "
			+ "LEFT JOIN HsFolderShare fs ON fs.folderId IS NULL "
			+ "LEFT JOIN HsFolder f ON f.id = fd.folderId "
			+ "LEFT JOIN Documents d ON fd.type = 'VAN_BAN_DEN' AND d.id = fd.docId AND d.active = TRUE "
			+ "LEFT JOIN DocumentOut d1 ON fd.type = 'VAN_BAN_DI' AND d1.id = fd.docId AND d1.active = TRUE "
			+ "WHERE (:folderId IS NULL OR fd.folderId = :folderId) "
			+ "AND (:status IS NULL OR f.status = :status) "
			+ "AND (:folderId IS NOT NULL OR (:folderId IS NULL AND (fd.createBy = :userId OR fs.userId = :userId)))"
			+ "AND fd.clientId = :clientId AND fd.active = TRUE")
	List<FolderDocumentDto> getListDocumentsByFolderId(Long folderId, HsFolderStatusEnum status, Long clientId);

	@Query("SELECT new com.vz.backend.business.dto.hstl.IconBasicDto(fd.docId, fd.type) FROM HsFolderDocument fd "
			+ "LEFT JOIN Documents d ON fd.type = :vanBanDen AND d.id = fd.docId AND d.active = :active "
			+ "LEFT JOIN DocumentOut d1 ON fd.type = :vanBanDi AND d1.id = fd.docId AND d1.active = :active "
			+ "WHERE fd.folderId = :folderId AND fd.clientId = :clientId AND (:active is null OR fd.active = :active)")
	List<IconBasicDto> getListDocIdByFolderId(Long folderId, DocumentTypeEnum vanBanDen, DocumentTypeEnum vanBanDi, Long clientId, Boolean active);

	@Query("SELECT new com.vz.backend.business.dto.hstl.DocumentReportDto(fd.id, fd.createDate, type) "
			+ "FROM HsFolderDocument fd "
			+ "WHERE active is TRUE AND createBy=:userId AND "
			+ "(coalesce(:startDate, null) is NULL or fd.createDate BETWEEN :startDate and :endDate)")
	List<DocumentReportDto> report(Date startDate, Date endDate, Long userId);

	boolean existsByCreateByAndTypeAndDocIdAndActiveTrue(Long userId, DocumentTypeEnum docType, Long docId);

	@Query("SELECT fd.folder FROM HsFolderDocument fd WHERE fd.docId = :docId AND fd.type = :docType AND fd.active = :active AND fd.clientId = :clientId")
	List<HsFolder> findFolderByDocIdAndTypeAndActive(Long docId, DocumentTypeEnum docType, boolean active, Long clientId);

	Long countByDocIdAndTypeAndActiveAndClientId(Long docId, DocumentTypeEnum docType, boolean active, Long clientId);

	@Query("SELECT new com.vz.backend.business.dto.hstl.FolderDetailBasicDto(fd.folderId, fd.folder.title, fd.folder.folderType, "
			+ "fd.folder.maintenanceObj.name, fd.folder.createDate, u.fullName, fd.folder.orgQL.name) "
			+ "FROM HsFolderDocument fd INNER JOIN User u ON u.id = fd.folder.createBy "
			+ "WHERE fd.docId = :docId AND fd.type = :docType AND fd.active = true AND fd.clientId = :clientId AND fd.folder.createBy =:userId")
	List<FolderDetailBasicDto> getHosoByDocIdAndDocType(Long userId, Long docId, DocumentTypeEnum docType, Long clientId);

	@Query("SELECT NEW com.vz.backend.business.dto.hstl.export.ContentDoc(fd) "
			+ "FROM HsFolderDocument fd WHERE fd.folderId=:folderId AND fd.clientId=:clientId")
	List<ContentDoc> getByFolderId(Long folderId, Long clientId);
	
	@Query("SELECT COUNT(1) "
			+ "FROM HsFolderDocument fd WHERE fd.folderId=:folderId AND fd.docId=:docId AND fd.type=:type")
	int isDuplicateKeyById(Long folderId, Long docId, DocumentTypeEnum type);

	@Query("SELECT COUNT(1) "
			+ "FROM HsFolderDocument fd WHERE fd.folderId IS NULL AND fd.docId=:docId AND fd.type=:type AND fd.clientId = :clientId AND fd.active = TRUE")
	long countByDocIdAndTypeAndFolderIdIsNull(Long docId, DocumentTypeEnum type, Long clientId);
}
