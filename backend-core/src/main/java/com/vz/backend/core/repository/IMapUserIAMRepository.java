package com.vz.backend.core.repository;

import com.vz.backend.core.domain.MapUserIAM;
import org.springframework.data.jpa.repository.Query;

public interface IMapUserIAMRepository extends IRepository<MapUserIAM>{

    @Query("SELECT m FROM MapUserIAM m WHERE m.subId = :subId AND m.active =:active")
    MapUserIAM findBySubIdIAM(String subId, Boolean active);

    @Query("SELECT m FROM MapUserIAM m WHERE m.userId = :userId AND m.active =:active and m.clientId =:clientId")
    MapUserIAM findByUserId(Long userId, Boolean active, Long clientId);

    @Query("SELECT count(*) > 0 FROM MapUserIAM m WHERE m.userId = :userId AND m.active =:active and m.clientId =:clientId")
    Boolean findUserMapByUserId(Long userId, Boolean active, Long clientId);
}
