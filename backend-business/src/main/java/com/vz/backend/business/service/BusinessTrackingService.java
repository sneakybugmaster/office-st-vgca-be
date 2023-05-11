package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Service;

import com.vz.backend.business.config.FolderTypeEnum;
import com.vz.backend.business.domain.BusinessTracking;
import com.vz.backend.business.domain.BusinessTracking.BusinessTrackingType;
import com.vz.backend.business.dto.BusinessTrackingDto;
import com.vz.backend.business.repository.IBusinessTrackingRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class BusinessTrackingService extends BaseService<BusinessTracking>  {

	@Autowired
	private IBusinessTrackingRepository btRepo;

	@Override
	public IRepository<BusinessTracking> getRepository() {
		return btRepo;
	}

	public void track(Long id, BusinessTrackingType type) {
		BusinessTracking track = btRepo.findOne(id, type);
		if (track == null) {
			track = new BusinessTracking(id, type);
		}
		track.count();
		btRepo.save(track);
	}

	public List<BusinessTrackingDto> listByType(BusinessTrackingType type) {
		Long userId = BussinessCommon.getUserId();

		Sort sort = Sort.by(Direction.DESC, "count");
		Pageable pageable = PageRequest.of(0, 10, sort);

		Page<BusinessTrackingDto> data;
		switch (type) {
		case VB_DEN:
			data = listByVbDen(userId, type, pageable);
			break;
		case VB_DI:
			data = listByVbDi(userId, type, pageable);
			break;
		case HO_SO:
			data = listByHoSo(userId, type, pageable);
			break;
		default:
			return new ArrayList<>();
		}

		if (data == null) {
			return new ArrayList<>();
		}
		return data.getContent();
	}

	public Page<BusinessTrackingDto> listByVbDen(Long userId, BusinessTrackingType type, Pageable pageable) {
		return btRepo.listByVbDen(userId, type, pageable);
	}

	public Page<BusinessTrackingDto> listByVbDi(Long userId, BusinessTrackingType type, Pageable pageable) {
		return btRepo.listByVbDi(userId, type, pageable);
	}

	public Page<BusinessTrackingDto> listByHoSo(Long userId, BusinessTrackingType type, Pageable pageable) {
		Long orgId = BussinessCommon.getOrgId();
		return btRepo.listByHoSo(FolderTypeEnum.CANHAN, FolderTypeEnum.COQUAN, userId, orgId, type, pageable);
	}

}
