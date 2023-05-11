package com.vz.backend.business.service.hstl;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.config.FolderPermissionEnum;
import com.vz.backend.business.domain.hstl.HsFolder;
import com.vz.backend.business.domain.hstl.HsFolderShare;
import com.vz.backend.business.dto.hstl.UserWithOrgAndPositionDto;
import com.vz.backend.business.repository.hstl.IHsFolderShareRepository;
import com.vz.backend.business.service.NotificationService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.config.NotificationHandleStatusEnum;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Repository
public class HsFolderShareService extends BaseService<HsFolderShare>{

	@Autowired
	private IHsFolderShareRepository hsFolderShareRepo;
	
	@Autowired
	private NotificationService notifyService;
	
	@Override
	public IRepository<HsFolderShare> getRepository() {
		return hsFolderShareRepo;
	}

	public void addForCreator(Long folderId) {
		HsFolderShare fs = new HsFolderShare(folderId, BussinessCommon.getUserId(), FolderPermissionEnum.FULL);
		hsFolderShareRepo.save(fs);
	}
	
	public Boolean add(List<HsFolderShare> input) {
		if (input != null && !input.isEmpty()) {
			hsFolderShareRepo.deleteByFolderId(input.get(0).getFolderId(), BussinessCommon.getClientId());
			hsFolderShareRepo.saveAll(input);
		}
		return true;
	}

	public List<UserWithOrgAndPositionDto> getListShare(Long folderId) {
		return hsFolderShareRepo.getListShare(folderId, BussinessCommon.getClientId());
	}
	
	public List<HsFolderShare> getByFolderId(Long folderId) {
		return hsFolderShareRepo.findByFolderIdAndClientIdAndActiveTrue(folderId, BussinessCommon.getClientId());
	}
	
	public void inActive(HsFolder folder) {
		List<HsFolderShare> shares = getByFolderId(folder.getId());
		shares.forEach(i -> i.setActive(false));
		
		List<Long> userIds = shares.stream().map(i->i.getUserId()).distinct().collect(Collectors.toList());
		notifyService.addAll(userIds, folder.getId(), folder.getTitle(), DocumentTypeEnum.TU_HO_SO,
				NotificationHandleStatusEnum.HS_CV_DUNG_CHIA_SE, ModuleCodeEnum.HSTL_CANHAN);
		
		hsFolderShareRepo.saveAll(shares);
	}
}
