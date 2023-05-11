package com.vz.backend.business.repository.hstl;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.hstl.Headings;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IHeadingsRepoository extends IRepository<Headings> {

	List<Headings> findByParentIdAndClientIdAndActiveTrue(Long id, Long clientId);

}
