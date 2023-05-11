package com.vz.backend.business.repository.outsideconnect;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.outsideconnect.TrackingConnectOutside;

@Repository
public interface ITrackingConnectOutsideRepository extends JpaRepository<TrackingConnectOutside, Long> {

	@Query("SELECT t FROM TrackingConnectOutside t ORDER BY t.createDate DESC")
	Page<TrackingConnectOutside> list(Pageable page);

}
