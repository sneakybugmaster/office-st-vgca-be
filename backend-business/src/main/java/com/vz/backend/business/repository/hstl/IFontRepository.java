package com.vz.backend.business.repository.hstl;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.hstl.Font;
import com.vz.backend.core.dto.LabelValueId;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IFontRepository extends IRepository<Font> {
	
	Font findByFondNameAndOrgIdAndClientIdAndActiveTrue(String name, Long orgId, Long clientId);

	Page<Font> findByOrgIdAndClientIdAndActiveTrue(Long orgId, Long clientId, Pageable pageable);

	@Query("SELECT NEW com.vz.backend.core.dto.LabelValueId(f.id, f.organld, f.fondName) FROM Font f WHERE f.orgId=:orgId AND f.active=TRUE AND f.clientId=:clientId")
	List<LabelValueId<String>> list(Long orgId, Long clientId);

	Font findByOrganldAndOrgIdAndClientIdAndActiveTrue(String organld, Long orgId, Long clientId);
}
