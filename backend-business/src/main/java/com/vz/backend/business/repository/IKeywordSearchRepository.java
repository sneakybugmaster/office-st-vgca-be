package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.KeywordSearch;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IKeywordSearchRepository extends IRepository<KeywordSearch> {
	@Query("SELECT DISTINCT k.key FROM KeywordSearch k WHERE k.clientId=:clientId AND k.active=TRUE AND k.userId=:userId AND k.key IS NOT NULL")
	List<String> getKeySearch(Long userId, Long clientId);

	KeywordSearch findByKeyAndUserIdAndClientIdAndActiveTrue(String key, Long userId, Long clientId);
}
