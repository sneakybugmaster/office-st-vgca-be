package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.WordEditorProcess;
import com.vz.backend.business.dto.ReportDocByTypeDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IWordEditorProcessRepository extends IRepository<WordEditorProcess>{

	@Query("select d from WordEditorProcess d "
			+ " where d.weId = :weId and d.clientId =:clientId and d.active = true and d.toUserId = :userId "
			+ " and d.step in (select max(d1.step) from WordEditorProcess d1 where d1.weId = :weId "
			+ " and d1.clientId =:clientId and d1.active = true and d1.toUserId = :userId group by d1.toUserId order by d.id)")
	List<WordEditorProcess> findByToUserAndWeId(Long weId, Long userId, Long clientId);

	@Query("select d from WordEditorProcess d "
			+ " where d.weId = :weId and d.clientId =:clientId and d.active = true and d.toUserId in (:userIds) "
			+ " and d.step = :step")
	List<WordEditorProcess> findByToUserAndWeId(Long weId, int step, Long[] userIds, Long clientId);

	List<WordEditorProcess> findByWeIdAndClientIdAndActiveTrue(Long weId, Long clientId);
	
	@Query("select count(1) > 0 from WordEditorProcess d "
			+ " where d.weId = :weId and d.clientId =:clientId and d.active = true and (d.toUserId = :userId OR d.frUserId = :userId) ")
	boolean isUserInThread(Long weId, Long userId, Long clientId);

	@Query("select new com.vz.backend.business.dto.ReportDocByTypeDto(concat(d.handleStatus, ''), count(distinct d.weId)) from WordEditorProcess d "
			+ " where d.clientId =:clientId and d.active = true and d.toUserId = :userId group by d.handleStatus")
	List<ReportDocByTypeDto> report(Long userId, Long clientId);
}
