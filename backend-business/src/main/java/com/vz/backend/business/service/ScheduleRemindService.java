package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.controller.ContentRemindDto;
import com.vz.backend.business.domain.ScheduleRemind;
import com.vz.backend.business.domain.ScheduleRemindIgnore;
import com.vz.backend.business.dto.MatchScheduleDto;
import com.vz.backend.business.dto.ScheduleRemindAssignedDto;
import com.vz.backend.business.dto.ScheduleRemindDto;
import com.vz.backend.business.repository.IScheduleRemindIgnoreRepository;
import com.vz.backend.business.repository.IScheduleRemindRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.config.NotificationHandleStatusEnum;
import com.vz.backend.core.config.ObjectRemindedTypeEnum;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.IdName;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.exception.RestForbidden;
import com.vz.backend.core.repository.ICategoryRepository;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.repository.IUserRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.util.DateTimeUtils;
import org.springframework.transaction.annotation.Transactional;

@Service
public class ScheduleRemindService extends BaseService<ScheduleRemind> {

	@Autowired
	private IScheduleRemindRepository scheduleRemindRepo;

	@Autowired
	private IScheduleRemindIgnoreRepository sriRepo;

	@Autowired
	private NotificationService notiService;

	@Autowired
	private IUserRepository userRepo;

	@Autowired
	private ICategoryRepository cateRepo;
	
	@Autowired
	private Calendar2Service calendarService;
	
	@Autowired
	private DocumentService docService;
	
	@Autowired
	private DocumentOutService docOutService;

	@Override
	public IRepository<ScheduleRemind> getRepository() {
		return scheduleRemindRepo;
	}

	public ScheduleRemind add(ScheduleRemindDto dto) {
		ScheduleRemind tmp = new ScheduleRemind(dto);
		return scheduleRemindRepo.save(tmp);
	}

	public ScheduleRemind update(ScheduleRemindDto dto) {
		// EntityNotFoundException
		ScheduleRemind tmp = scheduleRemindRepo.findByClientIdAndId(BussinessCommon.getClientId(), dto.getId());
		if (tmp == null) {
			return null;
		}
		tmp.put(dto);
		return scheduleRemindRepo.save(tmp);
	}

	public ScheduleRemind get(Long id) {
		return scheduleRemindRepo.findByClientIdAndId(BussinessCommon.getClientId(), id);
	}

	public List<ScheduleRemindAssignedDto> getAll(Boolean active) {
		return fillInfo(scheduleRemindRepo.getAll(BussinessCommon.getUserId(), active, BussinessCommon.getClientId()));
	}

	public List<ScheduleRemindAssignedDto> assigned() {
		User u = BussinessCommon.getUser();
		return fillInfo(scheduleRemindRepo.assigned(u.getId()));
	}

	@Transactional
	public void delete(Long id) {
		Long ownerId = scheduleRemindRepo.getOwner(id);
		if (ownerId == null) {
			throw new RestExceptionHandler(Message.NOT_EXIST_SCHEDULE + id);
		}
		if (!BussinessCommon.getUserId().equals(ownerId)) {
			throw new RestForbidden(Message.NOT_CREATE_SCHEDULE);
		}
		scheduleRemindRepo.deleteIgnore(id);
		scheduleRemindRepo.deleteById(id);
	}

	public void setActive(Long id, boolean active) {
		ScheduleRemind entity = scheduleRemindRepo.getOwnerFull(id);
		if (entity == null) {
			throw new RestExceptionHandler(Message.NOT_EXIST_SCHEDULE + id);
		}
		if (!BussinessCommon.getUserId().equals(entity.getCreateBy())) {
			throw new RestForbidden(Message.NOT_CREATE_SCHEDULE);
		}
		entity.setActive(active);
		scheduleRemindRepo.save(entity);
	}

	public void ignore(Long id) {
		ScheduleRemindIgnore sri = sriRepo.findByUserIdAndRemindId(BussinessCommon.getUserId(), id);
		if (sri == null) {
			sri = new ScheduleRemindIgnore(BussinessCommon.getUserId(), id);
		}
		sri.setActive(true);
		sriRepo.save(sri);
	}

	public void allow(Long id) {
		ScheduleRemindIgnore sri = sriRepo.findByUserIdAndRemindId(BussinessCommon.getUserId(), id);
		if (sri == null || Boolean.FALSE.equals(sri.getActive())) {
			return;
		}
		sri.setActive(false);
		sriRepo.save(sri);
	}

	@Transactional
	public List<MatchScheduleDto> notificationTask() {
		Calendar c = Calendar.getInstance(DateTimeUtils.timeZone());
		String hour = ScheduleRemind.wrap(c.get(Calendar.HOUR));
		String date = ScheduleRemind.wrap(c.get(Calendar.DATE));
		String dayOFWeek = ScheduleRemind.wrap(c.get(Calendar.DAY_OF_WEEK));
		String month = ScheduleRemind.wrap(c.get(Calendar.MONTH));
		List<MatchScheduleDto> listDto = scheduleRemindRepo.match(hour, date, dayOFWeek, month);
		notiService.deleteSchedule(c);
		listDto.forEach(dto -> {
			notiService.add2(dto.getUserId(), dto.getScheduleId(), dto.getPreview(), DocumentTypeEnum.NHAC_VIEC,
					NotificationHandleStatusEnum.NULL2, ModuleCodeEnum.SCHEDULE_REMIND_VIEW, dto.getClientId());
		});
		return listDto;

	}

//	private <T extends ScheduleRemindDto> T fillInfo(T data) {
//		this.fillInfo(Arrays.asList(data));
//		return data;
//	}

	private <T extends ScheduleRemindDto> List<T> fillInfo(List<T> data) {
		if (data == null) {
			return data;
		}
		Long clientId = BussinessCommon.getClientId();
		Set<Long> userIds = new HashSet<>();
		Set<Long> positionIds = new HashSet<>();
		data.forEach(dto -> {
			userIds.addAll(dto.getUserId());
			positionIds.addAll(dto.getPosition());
		});
		List<IdName> allUser = userRepo.findByIds(userIds, clientId);
		List<IdName> allPosition = cateRepo.findByIds(positionIds, clientId);
		Map<Long, String> allUserMap = new HashMap<>();
		Map<Long, String> allPositionMap = new HashMap<>();

		allUser.forEach(u -> allUserMap.put(u.getId(), u.getName()));
		allPosition.forEach(u -> allPositionMap.put(u.getId(), u.getName()));
		data.forEach(dto -> {
			dto.setAllUser(allUserMap);
			dto.setAllPosition(allPositionMap);
		});
		return data;
	}

	public List<ContentRemindDto> getContentByObjType(ObjectRemindedTypeEnum type) {
		switch (type) {
		case CALENDAR_MEETING:
			return ContentRemindDto.getByCalendar(calendarService.getToNow());
		case DOCUMENT_IN:
			return ContentRemindDto.getDocumentIn(docService.getByRemind());	
		case DOCUMENT_OUT:
			return ContentRemindDto.getDocumentOut(docOutService.getByRemind());
		default:
			return new ArrayList<>();
		}
	}

	public void setContent(ScheduleRemind entity) {
		List<Long> idList = ScheduleRemindDto.toListLong(entity.getObjId());
		List<ContentRemindDto> content = new ArrayList<>();
		switch (entity.getObjType()) {
		case CALENDAR_MEETING:
			content = ContentRemindDto.getByCalendar(calendarService.findByIds(idList));
			break;
		case DOCUMENT_IN:
			content = ContentRemindDto.getDocumentIn(docService.findByIds(idList));
			break;
		case DOCUMENT_OUT:
			content = ContentRemindDto.getDocumentOut(docOutService.findByIds(idList));
			break;
		default:
			break;
		}
		
		entity.setContent(content);
	}
}
