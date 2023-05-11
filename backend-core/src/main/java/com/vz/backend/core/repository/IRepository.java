package com.vz.backend.core.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.NoRepositoryBean;

import com.vz.backend.core.domain.RootModel;

import io.lettuce.core.dynamic.annotation.Param;

/**
 * @author DucND
 * @date Apr 14, 2020
 */
@NoRepositoryBean
public interface IRepository<T extends RootModel> extends JpaRepository<T, Long> {
	List<T> findByClientIdAndActive(Long clientId, boolean active);

	List<T> findByClientIdAndActive(Long clientId, boolean active, Pageable pageable);

	Page<T> findPageByClientIdAndActive(Long clientId, boolean active, Pageable pageable);

	T findByClientIdAndId(Long clientId, Long id);

	List<T> findByClientId(Long clientId);

	List<T> findByClientId(Long clientId, Pageable pageable);

	Page<T> findAllByClientId(Long clientId, Pageable pageable);

	List<T> findByClientId(Long clientId, Sort sort);

	@Query("SELECT t FROM #{#entityName} t WHERE t.clientId=:clientId AND (:active is null OR t.active=:active)")
	List<T> findByClientIdAndActive(@Param("clientId") Long clientId, @Param("active") Boolean active, Sort sort);

	Page<T> findByActiveAndClientId(boolean active, Long clientId, Pageable pageable);
}
