package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.AttachmentComment;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IAttachmentCommentRepository extends IRepository<AttachmentComment> {
	List<AttachmentComment> findByClientIdAndCommentId(Long client, Long cmtId);

	AttachmentComment findByNameAndClientIdAndActive(String name, Long clientId, boolean b);
}
