package com.vz.backend.business.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.domain.ScheduleRemind;
import com.vz.backend.business.dto.MatchScheduleDto;
import com.vz.backend.business.dto.ScheduleRemindAssignedDto;
import com.vz.backend.business.dto.ScheduleRemindDto;
import com.vz.backend.business.service.ScheduleRemindService;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.config.ObjectRemindedTypeEnum;
import com.vz.backend.core.dto.LabelValueDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.exception.RestForbidden;
import com.vz.backend.core.service.RoleService;

@RestController
@RequestMapping("/schedule-remind")
public class ScheduleRemindController {

	@Autowired
	private ScheduleRemindService scheduleRemindService;

	@Autowired
	private RoleService roleService;

	@PostMapping(value = "/")
	public ResponseEntity<ScheduleRemindDto> add(@RequestBody ScheduleRemindDto dto) {
		if (!dto.valid()) {
			throw new RestExceptionHandler("Vui lòng nhập đầy đủ thông tin hợp lệ");
		}
		checkPermission();
		return new ResponseEntity<>(new ScheduleRemindDto(scheduleRemindService.add(dto)), HttpStatus.OK);
	}

	@GetMapping(value = "/")
	public ResponseEntity<List<ScheduleRemindAssignedDto>> getAll(@RequestParam(required = false) Boolean active) {
		checkPermission();
		return new ResponseEntity<>(scheduleRemindService.getAll(active), HttpStatus.OK);
	}

	@GetMapping(value = "/assigned")
	public ResponseEntity<List<ScheduleRemindAssignedDto>> assign() {
		return new ResponseEntity<>(scheduleRemindService.assigned(), HttpStatus.OK);
	}

	@GetMapping(value = "/{id}")
	public ResponseEntity<ScheduleRemindDto> get(@PathVariable Long id) {
		checkPermission();
		ScheduleRemind entity = scheduleRemindService.get(id);
		if (entity == null) {
			throw new RestExceptionHandler("Không tồn tại Nhắc việc với id: " + id);
		}
		
		scheduleRemindService.setContent(entity);
		return new ResponseEntity<>(new ScheduleRemindDto(entity), HttpStatus.OK);
	}

	@DeleteMapping(value = "/{id}")
	public ResponseEntity<ScheduleRemindDto> delete(@PathVariable Long id) {
		checkPermission();
		scheduleRemindService.delete(id);
		return new ResponseEntity<>(HttpStatus.OK);
	}

	@PatchMapping(value = "/{id}")
	public ResponseEntity<ScheduleRemindDto> disable(@PathVariable Long id, @RequestParam boolean active) {
		checkPermission();
		scheduleRemindService.setActive(id, active);
		return new ResponseEntity<>(HttpStatus.OK);
	}

	@PutMapping(value = "/")
	public ResponseEntity<ScheduleRemindDto> update(@RequestBody ScheduleRemindDto dto) {
		Long id = dto.getId();

		if (id == null) {
			throw new RestExceptionHandler("Thiếu id của nhắc việc");
		}
		if (!dto.valid()) {
			throw new RestExceptionHandler("Vui lòng nhập đầy đủ thông tin");
		}

		checkPermission();
		ScheduleRemind entity = scheduleRemindService.update(dto);
		if (entity == null) {
			throw new RestExceptionHandler("Không tồn tại Nhắc việc với id: " + id);
		}
		return new ResponseEntity<>(new ScheduleRemindDto(entity), HttpStatus.OK);
	}

	@PostMapping(value = "/ignore/{id}")
	public ResponseEntity<ScheduleRemindDto> ignore(@PathVariable Long id) {
		scheduleRemindService.ignore(id);
		return new ResponseEntity<>(HttpStatus.OK);
	}

	@DeleteMapping(value = "/ignore/{id}")
	public ResponseEntity<ScheduleRemindDto> allow(@PathVariable Long id) {
		scheduleRemindService.allow(id);
		return new ResponseEntity<>(HttpStatus.OK);
	}

	@PostMapping(value = "/test")
	public ResponseEntity<List<MatchScheduleDto>> test() {
		try {
			scheduleRemindService.notificationTask();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return new ResponseEntity<>(HttpStatus.OK);
	}

	private void checkPermission() {
		if (roleService.existUserInModule(ModuleCodeEnum.SCHEDULE_REMIND_VIEW.getName())) {
			return;
		}
		throw new RestForbidden("Bạn không có quyền truy cập vào module nhắc việc");
	}
	
	@GetMapping(value = "/objType")
	public ResponseEntity<List<LabelValueDto<String>>> getObjType() {
		return new ResponseEntity<>(ObjectRemindedTypeEnum.CALENDAR_MEETING.get(), HttpStatus.OK);
	}
	
	@GetMapping(value = "/content")
	public ResponseEntity<List<ContentRemindDto>> getContentByObjType(
			@RequestParam(defaultValue = "CALENDAR_MEETING", required = true) ObjectRemindedTypeEnum type) {
		return new ResponseEntity<>(scheduleRemindService.getContentByObjType(type), HttpStatus.OK);
	}
}
