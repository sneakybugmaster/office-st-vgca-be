package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.Fields;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IFieldsRepository extends IRepository<Fields> {
	@Query(value = "select f  from Fields f where f.name = :name and f.catId = :catId and f.clientId = :idClient")
	Fields getFieldByNameAndCatIdAndClientId(@Param("name") String name, @Param("catId") Long catId,
			@Param("idClient") Long idClient);

	List<Fields> findByClientIdAndCatId(Long clientId, Long catId);
}
