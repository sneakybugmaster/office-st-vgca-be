package com.vz.backend.core.service;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.MapUserIAM;
import com.vz.backend.core.repository.IMapUserIAMRepository;
import com.vz.backend.core.repository.IRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class MapUserIAMService extends BaseService<MapUserIAM> {
    @Override
    public IRepository<MapUserIAM> getRepository() {
        return null;
    }

    @Autowired
    private IMapUserIAMRepository iMapUserIAMRepository;

    public MapUserIAM save(MapUserIAM mapUserIAM) {
        mapUserIAM = iMapUserIAMRepository.save(mapUserIAM);
        return mapUserIAM;
    }

    public MapUserIAM getMapUserBySubId(String subId) {
        return iMapUserIAMRepository.findBySubIdIAM(subId, true);
    }

    public MapUserIAM getMapUserByUserId() {
        return iMapUserIAMRepository.findByUserId(BussinessCommon.getUserId(), true, BussinessCommon.getClientId());
    }

    public Boolean findUserMapByUserId() {
        return iMapUserIAMRepository.findUserMapByUserId(BussinessCommon.getUserId(), true, BussinessCommon.getClientId());
    }
}
