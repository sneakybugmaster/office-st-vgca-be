package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.Comment;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface ICommentRepository extends IRepository<Comment> {

	List<Comment> findByObjIdAndObjTypeAndClientIdAndActiveTrue(Long weId, DocumentTypeEnum vanBanSoanThao, Long clientId);

}
