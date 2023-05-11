package com.vz.backend.core.repository;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.vz.backend.core.domain.Ldap;

@Repository
public interface ILdapRepository extends IRepository<Ldap> {
	List<Ldap> findByActive(boolean active);
	
	Ldap findFirstByActive(boolean active);
}
