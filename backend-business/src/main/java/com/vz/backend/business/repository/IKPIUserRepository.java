package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.vz.backend.business.config.ReceiveTypeEnum;
import com.vz.backend.business.domain.KPIUser;
import com.vz.backend.core.repository.IRepository;
@Repository
public interface IKPIUserRepository extends IRepository<KPIUser>{

	List<KPIUser> findByUserIdAndKpiAppIdAndClientIdAndActiveTrue(Long userId, Long kpiAppId, Long clientId);

	List<KPIUser> findByUserIdInAndKpiAppIdAndTypeAndClientIdAndActiveTrue(List<Long> userIds, Long kpiAppId, ReceiveTypeEnum org, Long clientId);

}
