package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import com.vz.backend.business.domain.Calendar2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.business.domain.AttachmentCalendar;
import com.vz.backend.business.repository.IAttachmentCalendarRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ObjTypeEnum;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.EncryptionService;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.util.DateTimeUtils;

@Service
public class AttachmentCalendarService extends BaseService<AttachmentCalendar>{

	@Autowired
	IAttachmentCalendarRepository attRepository;
	
	@Autowired
	FilesStorageService storageService;
	
	@Autowired
	CalendarJoin2Service cJoinService;
	
	@Autowired
	Calendar2Service calendarService;
	
	@Autowired
	EncryptionService encryptService;
	
	@Autowired
	WordEditorService wordEditorService;
	
	@Override
	public IRepository<AttachmentCalendar> getRepository() {
		return null;
	}
	
	public List<AttachmentCalendar> addListAttachment(MultipartFile[] files, Long objId, ObjTypeEnum objType, int week, int year) {
		if (BussinessCommon.isEmptyArr(files)) {
			return Collections.emptyList();
		}
		List<AttachmentCalendar> aList = new ArrayList<>();
		for (MultipartFile f : files) {
			AttachmentCalendar a = new AttachmentCalendar();
			a.setName(storageService.save(f));
			a.setType(f.getContentType());
			a.setSize(f.getSize());
			a.setObjId(objId);
			a.setObjType(objType);
			a.setWeek(week);
			a.setYear(year);
			aList.add(a);
		}
		return attRepository.saveAll(aList);
	}
	
	@Override
	public void deleteById(Long id) {
		Optional<AttachmentCalendar> optional = attRepository.findById(id);
		if (!optional.isPresent()) {
			throw new RestExceptionHandler("Can't find attachment with id: " + id);
		}
		AttachmentCalendar a = optional.get();
		if (a == null) return;
		if (Boolean.TRUE.equals(a.getEncrypt())) {
			encryptService.delEncrypt(a.getName());
		} else {
			storageService.deleteFile(a.getName());
		}
		attRepository.delete(a);
	}
	
	public List<AttachmentCalendar> getByObjId(Long objId, ObjTypeEnum type) {
		return attRepository.findByObjIdAndObjType(objId, type, BussinessCommon.getClientId());
	}
	
	public AttachmentCalendar validDownloadFile(String name) {
		AttachmentCalendar a = attRepository.findByNameAndClientIdAndActiveTrue(name, BussinessCommon.getClientId());
		if (a == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}
		Calendar2 c = calendarService.findByIdAndClientIdAndActive(a.getObjId(),true);
		if((ObjTypeEnum.CALENDAR.equals(a.getObjType())
				&& !calendarService.isMemberCalendar(a.getObjId())) ) {
			if(!calendarService.showAttachmentCalendar(c, BussinessCommon.getUser().getOrg(), c.getCreateBy())){
				throw new RestExceptionHandler(Message.NO_ACTION_PERMISSION);
			}
		}
		return a;
	}
	
	public List<AttachmentCalendar> getByObjIds(List<Long> objId, ObjTypeEnum type) {
		return attRepository.findByObjTypeAndClientIdAndActiveTrueAndObjIdIn(type, BussinessCommon.getClientId(), objId);
	}
	
	public AttachmentCalendar getAttCalWeek(Date startTime) {
		if (startTime == null)
			return null;
		int week = DateTimeUtils.getWeekOfYear(startTime);
		int year = DateTimeUtils.getYear(startTime);
		return attRepository.findByClientIdAndWeekAndYearAndObjTypeAndActiveTrue(BussinessCommon.getClientId(), week,
				year, ObjTypeEnum.CALENDAR_WEEK);
	}
	
	public AttachmentCalendar getAttCalWeek(int week, int year) {
		return attRepository.findByClientIdAndWeekAndYearAndObjTypeAndActiveTrue(BussinessCommon.getClientId(), week,
				year, ObjTypeEnum.CALENDAR_WEEK);
	}
	
	/**
	 * for category only save 1 file
	 * @param objId
	 * @param objType
	 * @return
	 */
	public Resource downloadByIdCat(Long objId) {
		List<AttachmentCalendar> aList = getByObjId(objId, ObjTypeEnum.MODULE);
		if(!aList.isEmpty()) {
			return storageService.load(aList.get(0).getName());
		}
		throw new RestExceptionHandler(Message.NOT_FOUND_OBJECT);
	}

	public List<String> getExtensionLoadByIdCat(Long objId) {
		List<AttachmentCalendar> aList = getByObjId(objId, ObjTypeEnum.MODULE);
		if(!aList.isEmpty()) {
			List<String> list =  new ArrayList<>();
			list.add(aList.get(0).getName());
			return list;
		}
		throw new RestExceptionHandler(Message.NOT_FOUND_OBJECT);
	}

	/**
	 * for category only update 1 file
	 * @param objId
	 * @param objType
	 * @return
	 */
	public AttachmentCalendar updateByIdCat(Long objId, MultipartFile file) {
		List<AttachmentCalendar> aList = getByObjId(objId, ObjTypeEnum.MODULE);
		if (!aList.isEmpty()) {
			AttachmentCalendar old = aList.get(0);
			String nName = storageService.replace(file, old.getName());
			old.setName(nName);
			return attRepository.save(old);
		}

		throw new RestExceptionHandler(Message.NOT_FOUND_OBJECT);
	}
	
	/**
	 * for category only update 1 file
	 * @param objId
	 * @param objType
	 * @return
	 */
	public void delByIdCat(Long objId) {
		List<AttachmentCalendar> aList = getByObjId(objId, ObjTypeEnum.MODULE);
		if (!aList.isEmpty()) {
			AttachmentCalendar old = aList.get(0);
			old.setActive(false);
			attRepository.save(old);
		} else {
			throw new RestExceptionHandler(Message.NOT_FOUND_OBJECT);
		}
		
	}

	public AttachmentCalendar add(MultipartFile encrypted, Long objId, String encryptName, ObjTypeEnum type) {
		if (ObjTypeEnum.CALENDAR.equals(type)) {
			calendarService.valid(objId, Message.NOT_FOUND_OBJECT);
		}
		
		if (ObjTypeEnum.WORD_EDITOR.equals(type)) {
			wordEditorService.valid(objId, Message.NOT_FOUND_OBJECT);
		}
		return attRepository.save(new AttachmentCalendar(encrypted, objId, encryptName, type));
	}
}
