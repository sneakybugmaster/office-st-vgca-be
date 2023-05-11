package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.TaskAttachment;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface ITaskAttachmentRepository extends IRepository<TaskAttachment> {

	TaskAttachment findByNameAndClientIdAndActive(String name, Long clientId, boolean b);

	TaskAttachment findByObjectId(Long commentId);

	@Query("select t from TaskAttachment t where t.objectId = :objId and t.typeObj = :type and t.active = :active and t.clientId = :clientId")
	List<TaskAttachment> getByObjId(Long objId, Long type, Long clientId, boolean active);

	@Query("select t from TaskAttachment t where t.objectId in (:cmtIds) and t.typeObj = :type and t.active = :active and t.clientId = :clientId")
	List<TaskAttachment> getByCmtIds(List<Long> cmtIds, Long type, Long clientId, boolean active);

	Page<TaskAttachment> findByClientIdAndObjectIdAndCreateByAndTypeObjAndActive(Long clientId, long object, Long userId, long l,
			boolean b, Pageable pageable);
}
