package com.vz.backend.business.service.outsideconnect;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.outsideconnect.TrackingConnectOutside;
import com.vz.backend.business.repository.outsideconnect.ITrackingConnectOutsideRepository;

@Service
public class TrackingConnectOutsideService {

	@Autowired
	private ITrackingConnectOutsideRepository trackingRepository;
	
	public TrackingConnectOutside save(TrackingConnectOutside data) {
		return trackingRepository.save(data);
	}
	
	public Page<TrackingConnectOutside> list(Pageable page) {
		return trackingRepository.list(page);
	}
}
