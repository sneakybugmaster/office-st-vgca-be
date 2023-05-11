package com.vz.backend.business.repository;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.vz.backend.business.domain.Notification;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.config.NotificationHandleStatusEnum;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface INotificationRepository extends IRepository<Notification> {

	Page<Notification> findByActiveAndUserIdOrderByIdDesc(Boolean active, Long userId, Pageable castToPageable);

	@Query("SELECT n FROM Notification n WHERE n.id IN (SELECT max(id) FROM Notification t "
			+ "WHERE t.userId = :userId and t.active = :active and t.system is null GROUP BY t.docId) "
			+ "ORDER BY n.id DESC")
	List<Notification> getByActiveAndUserId(Boolean active, Long userId);

	@Query("SELECT COUNT(n) FROM Notification n WHERE n.id IN (SELECT max(id) FROM Notification t "
			+ "WHERE t.userId = :userId AND t.read = :read AND t.active = :active AND t.system is null GROUP BY t.docId) ")
	Long countByActiveAndUserIdAndRead(Boolean active, Long userId, Boolean read);

	
	@Modifying()
	@Query("UPDATE Notification n SET n.active = false WHERE n.userId = :userId AND n.system is null")
	void deactiveAllByUserId(Long userId);

	
	@Modifying()
	@Query("UPDATE Notification n SET n.active = false WHERE n.id IN (:listIds)")
	void deactiveAllByIds(Long[] listIds);

	@Query("SELECT n from Notification n WHERE n.docId = :docId AND n.docType = :docType")
	List<Notification> deactiveAllByDocIdAndDocType(Long docId, DocumentTypeEnum docType);

	@Modifying()
	@Query("UPDATE Notification n SET n.active = :active WHERE n.docId = :docId AND n.docType = :docType")
	void setActiveByDocIdAndDocType(Long docId, DocumentTypeEnum docType, Boolean active);

	@Modifying()
	@Query("UPDATE Notification n SET n.active = :active WHERE n.docId = :docId and n.userId IN (:listUsers) AND n.docType = :docType")
	void setActiveByListUserIdAndDocIdAndDocType(List<Long> listUsers, Long docId, DocumentTypeEnum docType,
			boolean active);

	@Modifying()
	@Query("UPDATE Notification n SET n.active = :active WHERE n.docId = :docId and n.userId = :userId AND n.docType = :docType")
	void setActiveByUserIdAndDocIdAndDocType(Long userId, Long docId, DocumentTypeEnum docType, boolean active);

	@Modifying()
	@Query("UPDATE Notification n SET n.active = :active WHERE n.docId = :docId and n.userId = :userId AND n.docType = :docType AND n.docStatus IN (:listStatus)")
	void setActiveByDocIdAndDocTypeAndUserIdAndDocStatus(boolean active, Long docId, DocumentTypeEnum docType,
			Long userId, List<NotificationHandleStatusEnum> listStatus);


	@Modifying()
	@Query("SELECT n FROM Notification n  WHERE n.docId = :docId and n.userId = :userId AND n.docType = :docType")
	List<Notification> setReadByDocIdAndDocTypeAndUserId(Long docId, DocumentTypeEnum docType, Long userId);


	@Modifying()
	@Query("UPDATE Notification n SET n.read = :read WHERE n.id = :id AND n.userId = :userId")
	void setReadByIdAndUserId(boolean read, Long id, Long userId);

	@Query("SELECT n FROM Notification n WHERE n.userId = :userId and n.active = :active and n.docId=:docId and n.docType = :docType and n.docStatus = (:docStatus) and n.moduleCode = :module ORDER BY n.id DESC")
	List<Notification> getByUserId(Long userId, Long docId, DocumentTypeEnum docType, boolean active,
			NotificationHandleStatusEnum docStatus, ModuleCodeEnum module);


	@Modifying()
	@Query("DELETE FROM Notification n WHERE n.moduleCode=:moduleCode AND n.createDate BETWEEN :last AND :now")
	void deleteByCreateDateAndModuleCode(Date last, Date now, ModuleCodeEnum moduleCode);

}
