package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.config.ReceiveTypeEnum;
import com.vz.backend.business.domain.KPIUser;
import com.vz.backend.business.domain.Targets;
import com.vz.backend.business.repository.IKPIUserRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.UserService;

@Service
public class KPIUserSevice extends BaseService<KPIUser> {

	@Autowired
	IKPIUserRepository kpiUserRepository;
	
	@Autowired
	UserService userService;

	@Override
	public IRepository<KPIUser> getRepository() {
		return kpiUserRepository;
	}

	public List<KPIUser> save(Long appId, List<Long> userIds, List<Targets> kpis, ReceiveTypeEnum type) {
		List<KPIUser> kpiUsers = new ArrayList<>();
		if (BussinessCommon.isEmptyList(userIds) || BussinessCommon.isEmptyList(kpis))
			return kpiUsers;
		userIds.forEach(i -> kpis.forEach(j -> kpiUsers.add((new KPIUser(i, j.getId(), appId, type)))));
		return kpiUserRepository.saveAll(kpiUsers);
	}
	
	public void update(Map<KPIUser, KPIUser> oMap, Long appId, List<Long> userIds, List<Targets> kpis, ReceiveTypeEnum type) {
		Set<KPIUser> kpiUsers = new HashSet<>();
		if (BussinessCommon.isEmptyList(userIds) || BussinessCommon.isEmptyList(kpis))
			return;
		userIds.forEach(i -> kpis.forEach(j -> kpiUsers.add((new KPIUser(i, j.getId(), appId, type)))));
		kpiUsers.removeIf(i -> oMap.containsKey(new KPIUser(i.getUserId(), i.getTargetId(), i.getKpiAppId())));
		kpiUserRepository.saveAll(kpiUsers);
	}

	public List<KPIUser> save(List<KPIUser> kpis) {
		kpis.forEach(i -> i.valids());
		try {
			return kpiUserRepository.saveAll(kpis);
		} catch (Exception e) {
			e.printStackTrace();
			throw new RestExceptionHandler(Message.ERROR_SYS);
		}
	}
	
	public List<KPIUser> getByUserIdAndKPIAppId(Long userId, Long kpiAppId) {
		return kpiUserRepository.findByUserIdAndKpiAppIdAndClientIdAndActiveTrue(userId, kpiAppId, BussinessCommon.getClientId());
	}
	
	public List<KPIUser> getByUserIdAndKPIAppId(List<Long> userIds, Long kpiAppId) {
		return kpiUserRepository.findByUserIdInAndKpiAppIdAndTypeAndClientIdAndActiveTrue(userIds, kpiAppId, ReceiveTypeEnum.ORG, BussinessCommon.getClientId());
	}
	
	public void delByOrg(List<Long> orgIds, Long kpiApp) {
		List<Long> userIds = userService.findByOrgIds(orgIds);
		if(userIds.isEmpty()) return;
		List<KPIUser> kpiUsers = getByUserIdAndKPIAppId(userIds, kpiApp);
		for (KPIUser i : kpiUsers) {
			i.setActive(false);
			kpiUserRepository.save(i);
		}
	}
}
