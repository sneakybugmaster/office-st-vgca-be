package com.vz.backend.core.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import com.vz.backend.core.domain.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.AuthorityEnum;
import com.vz.backend.core.domain.AuthorityUser;
import com.vz.backend.core.dto.UserInfoDto;
import com.vz.backend.core.dto.UserTreeDto;
import com.vz.backend.core.repository.IAuthorityUserRepository;
import com.vz.backend.core.repository.IRepository;

@Service
public class AuthorityUserService extends BaseService<AuthorityUser> {

	@Override
	public IRepository<AuthorityUser> getRepository() {
		return null;
	}
	
	@Autowired
	IAuthorityUserRepository authorityRepository;
		
	public List<AuthorityUser> add(Long userId, List<AuthorityUser> newsList, Long positionId) {
		List<AuthorityUser> rsList = new ArrayList<>();
		List<AuthorityUser> oldList = get(userId, true, positionId);
		if(userId != null) newsList.forEach(i-> i.setUserId(userId));
		if(positionId != null) newsList.forEach(i-> i.setPositionId(positionId));
		
		List<AuthorityUser> all = new ArrayList<>();
		all.addAll(oldList);
		all.addAll(newsList);
		
		all.forEach(a -> {
			if (!oldList.contains(a) && newsList.contains(a)) {
				// add
				rsList.add(a);
			} else if (oldList.contains(a) && !newsList.contains(a)) {
				// del
				a.setActive(false);
				rsList.add(a);
			}
		});
		
		return authorityRepository.saveAll(rsList);
	}
	
	public List<AuthorityUser> get(Long userId, Boolean active, Long positionId ) {
		return authorityRepository.findByUserIdAndClientIdAndActive(userId, positionId, BussinessCommon.getClientId(), active);
	}
	
	public List<AuthorityUser> get(Long userId, Long clientId, Boolean active ) {
		return authorityRepository.findByUserIdAndClientIdAndActive(userId, null, clientId, active);
	}
	
	public boolean isUserHasAuthority(Long userId, Long positionId, AuthorityEnum authority) {
		List<AuthorityUser> authoritys = get(userId, true, positionId);
		Optional<AuthorityUser> o = authoritys.stream().filter(i->authority.equals(i.getAuthority())).findFirst();
		return o.isPresent();
	}

	public List<Long> getListUserApprovalByOrgIdAndAuthority(Long orgId, AuthorityEnum autho) {
		return authorityRepository.getListUserApprovalByOrgIdAndAuthority(orgId, autho, BussinessCommon.getClientId());
	}
	
	public List<UserTreeDto> getByAuthority(AuthorityEnum authority) {
		return authorityRepository.getByAuthority(authority, BussinessCommon.getClientId());
	}
	public List<User> getListUserTBANDPTB(List<String> list, String text, Long orgId) {
		return authorityRepository.getListUserTBANDPTB(list, BussinessCommon.getClientId(),text, orgId);
	}

	public List<UserInfoDto> getListLDCuc() {
		return authorityRepository.getByAuthority1(AuthorityEnum.LEADERSHIP, BussinessCommon.getClientId());
	}
	
	public Set<Long> checkAuthorByIds(Set<Long> idUs, AuthorityEnum authority) {
		return authorityRepository.checkAuthorByIds(idUs, authority, BussinessCommon.getClientId());
	}
	
	public List<AuthorityUser> getByPositionIds(List<Long> positionIds) {
		return authorityRepository.getByPositionIds(positionIds, BussinessCommon.getClientId());
	}
}
