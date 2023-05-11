package com.vz.backend.core.repository;

import com.vz.backend.core.domain.Secretary;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ISecretaryRepository extends IRepository<Secretary> {

	@Query("SELECT s FROM Secretary s WHERE s.bossId =:bossId AND s.clientId = :clientId AND (:active IS NULL OR s.active = :active)")
	List<Secretary> findByBossIdAndClientIdAndActive(Long bossId, Long clientId, Boolean active);
	
	@Modifying
	void deleteByBossIdAndUserIdIn(Long bossId, List<Long> userIds);
}
