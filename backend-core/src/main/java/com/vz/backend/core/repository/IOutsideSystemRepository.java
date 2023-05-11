package com.vz.backend.core.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import com.vz.backend.core.domain.OutsideSystem;

@Repository
public interface IOutsideSystemRepository extends IRepository<OutsideSystem> {

	Page<OutsideSystem> findAll(Pageable page);

	OutsideSystem findFirstByNameAndActiveTrue(String name);

	OutsideSystem findByDomainAndKeyAndActiveTrue(String domain, String key);
}
