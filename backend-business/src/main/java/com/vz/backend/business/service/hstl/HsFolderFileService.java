package com.vz.backend.business.service.hstl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.business.config.FolderPermissionEnum;
import com.vz.backend.business.domain.hstl.HsFolderFile;
import com.vz.backend.business.dto.hstl.ecm.FolderAttachmentDto;
import com.vz.backend.business.dto.hstl.export.ContentDoc;
import com.vz.backend.business.repository.hstl.IHsFolderFileRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.FilesStorageService;

@Service
public class HsFolderFileService extends BaseService<HsFolderFile> {

	@Autowired
	IHsFolderFileRepository hsFileRepo;

	@Autowired
	FilesStorageService storageService;
	
	@Autowired
	private HsFolderService hsFolderService;

	@Override
	public IRepository<HsFolderFile> getRepository() {
		return hsFileRepo;
	}
	
	public HsFolderFile update(Long id, HsFolderFile input) {
		HsFolderFile old = valid(id, Message.NOT_FOUND_DOC);
		input.valids();
		old.set(input);
		return hsFileRepo.save(old);
	}
	
	public HsFolderFile add(@NonNull Long id, @NonNull MultipartFile file) {
		HsFolderFile input = valid(id, Message.NOT_FOUND_DOC);
		hsFolderService.validatePermission(input.getFolderId(), FolderPermissionEnum.FULL, false);
		input.set(file, storageService.saveHs(file));
		input = hsFileRepo.save(input);
		hsFolderService.increaseTotalItems(input.getFolderId(), 1L);
		return input;
	}

	public HsFolderFile add(@NonNull HsFolderFile input, @NonNull MultipartFile file) {
		hsFolderService.validatePermission(input.getFolderId(), FolderPermissionEnum.FULL, false);
		input.setFileName(storageService.saveHs(file));
		input.setFileType(file.getContentType());
		input.setFileSize(file.getSize());
		input = hsFileRepo.save(input);
		hsFolderService.increaseTotalItems(input.getFolderId(), 1L);
		return input;
	}

	public Boolean deleteFileById(Long id) {
		try {
			HsFolderFile file = hsFileRepo.findByClientIdAndId(BussinessCommon.getClientId(), id);
			if (file == null) throw new RestExceptionHandler(Message.ACTION_FAILED);
			hsFolderService.validatePermission(file.getFolderId(), FolderPermissionEnum.FULL, false);
			hsFileRepo.deleteById(id);
			hsFolderService.decreaseTotalItems(file.getFolderId(), 1L);
		} catch (Exception e) {
			throw new RestExceptionHandler(Message.ACTION_FAILED);
		}
		return true;
	}
	
	public List<ContentDoc> getByFolderId(Long folderId) {
		return hsFileRepo.getByFolderId(folderId, BussinessCommon.getClientId());
	}
	
	public List<FolderAttachmentDto> getByFolderIds(List<Long> ids) {
		return hsFileRepo.getByFolderIds(ids, BussinessCommon.getClientId());
	}
}
