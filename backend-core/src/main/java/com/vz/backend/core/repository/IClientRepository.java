package com.vz.backend.core.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.vz.backend.core.domain.Client;

@Repository
public interface IClientRepository extends JpaRepository<Client, Long> {
	Client findByCodeAndActive(String code, boolean active);

	Client findByName(String name);
}
