package com.vz.backend.core.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.core.domain.Ldap;
import com.vz.backend.core.repository.ILdapRepository;
import com.vz.backend.core.repository.IRepository;

@Service
public class LdapService extends BaseService<Ldap> {

	@Autowired
	private ILdapRepository ldapRepository;

	@Override
	public IRepository<Ldap> getRepository() {
		return ldapRepository;
	}

	public List<Ldap> findByActive(boolean active) {
		return ldapRepository.findByActive(active);
	}
	
	public Ldap findActiveSt(boolean active) {
		return ldapRepository.findFirstByActive(active);
	}
}
