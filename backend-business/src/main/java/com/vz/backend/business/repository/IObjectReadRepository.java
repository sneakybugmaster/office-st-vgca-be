package com.vz.backend.business.repository;

import com.vz.backend.business.domain.ObjectRead;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.repository.IRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface IObjectReadRepository extends IRepository<ObjectRead> {

    ObjectRead findFirstByClientIdAndUserIdAndObjIdAndTypeAndActiveTrue(Long clientId, Long userId, Long objId,
                                                                        DocumentTypeEnum type);

    List<ObjectRead> findByClientIdAndUserIdAndObjIdInAndTypeAndActiveTrue(Long clientId, Long userId,
                                                                           List<Long> objIds, DocumentTypeEnum type);

    List<ObjectRead> findAllByObjIdInAndUserIdInAndTypeAndClientIdAndActive(List<Long> objIds, List<Long> userIds, DocumentTypeEnum type, long clientId, boolean active);

    long countByClientIdAndUserIdAndObjIdInAndTypeAndReadTrueAndActiveTrue(Long clientId, Long userId,
                                                                           List<Long> objIds, DocumentTypeEnum type);
}
