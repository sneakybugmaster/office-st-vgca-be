package com.vz.backend.business.service.outsideconnect;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.outsideconnect.TrackingObjectOutside;
import com.vz.backend.business.dto.outsideconnect.SentDataDto;
import com.vz.backend.business.repository.outsideconnect.ITrackingObjectOutsideRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class TrackingObjectOutsideService extends BaseService<TrackingObjectOutside> {

	@Autowired
	private ITrackingObjectOutsideRepository trackingRepository;

	@Override
	public IRepository<TrackingObjectOutside> getRepository() {
		return trackingRepository;
	}

	public Page<TrackingObjectOutside> list(Long objId, DocumentTypeEnum type, Pageable page) {
		return trackingRepository.list(objId, type, BussinessCommon.getClientId(), page);
	}

	/**
	 * For checked data that this object was sent
	 * 
	 * @param objId
	 * @param type
	 * @return
	 */
	public List<SentDataDto> getSentData(Long objId, DocumentTypeEnum type) {
		return trackingRepository.getSentData(objId, type, BussinessCommon.getClientId());
	}

	public TrackingObjectOutside getOutsideIdSendObjId(Long objId, DocumentTypeEnum type) {
		List<TrackingObjectOutside> rs = trackingRepository.getOutsideSendObjId(objId, type,
				BussinessCommon.getClientId());
		return rs.isEmpty() ? null : rs.get(0);
	}
}
