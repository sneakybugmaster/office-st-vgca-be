package com.vz.backend.business.repository;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.WordEditor;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IWordEditorRepository extends IRepository<WordEditor>{

	@Query("SELECT new com.vz.backend.business.dto.WordEditorDto(we.id, we.createBy, we.name, we.startDate, we.endDate, we.status, p.handleStatus, we.cat.name) "
			+ " FROM WordEditor we LEFT JOIN WordEditorProcess p ON we.id = p.weId "
			+ " WHERE (:text is null or lower(we.name) like %:text% or lower(we.description) like %:text%) "
			+ "	AND (:status is null or we.status = :status ) "
			+ " AND (coalesce (:startDate, null) is null or we.startDate > :startDate) "
			+ " AND (coalesce (:endDate, null) is null or we.endDate < :endDate) "
			+ "	AND (p.toUserId = :userId OR p.frUser = :userId) AND p.active = TRUE AND we.active = TRUE "
			+ " AND p.clientId=:clientId AND we.clientId=:clientId "
			+ " AND (we.id, p.step) in (select p1.weId, max(p1.step) from WordEditorProcess p1 where p1.active= true and p1.clientId = :clientId and (p1.toUserId = :userId) group by p1.weId )"
			+ " GROUP BY we.id, we.createBy, we.name, we.startDate, we.endDate, we.status, p.handleStatus, we.cat.name ")
	Page<WordEditor> list(String text, Date startDate, Date endDate, DocumentStatusEnum status, Long userId, Long clientId, Pageable pageable);

	@Query("SELECT we FROM WordEditor we WHERE we.clientId=:clientId and we.active = true and we.id in (:weIds) ")
	List<WordEditor> findByIds(List<Long> weIds, Long clientId);
	
	@Query("SELECT DISTINCT(we) FROM WordEditor we LEFT JOIN WordEditorProcess p ON we.id = p.weId WHERE we.clientId=:clientId and we.active = true and (p.toUserId =:userId or p.frUserId=:userId) and we.status = 'DONE'")
	List<WordEditor> findByIdss(Long clientId, Long userId);

	List<WordEditor> findByTaskIdAndClientIdAndActiveTrue(Long taskId, Long clientId);
}
