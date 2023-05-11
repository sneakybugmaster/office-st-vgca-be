package com.vz.backend.business.controller;

import java.util.*;

import com.vz.backend.business.domain.CalendarComment;
import com.vz.backend.business.domain.ecabinet.AttachmentMeeting;
import com.vz.backend.business.repository.ICalendar2Repository;
import com.vz.backend.business.service.CalendarCommentService;
import com.vz.backend.core.util.StreamUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

import com.vz.backend.business.config.ecabinet.FileTypeEnum;
import com.vz.backend.business.domain.AttachmentComment;
import com.vz.backend.business.domain.Calendar2;
import com.vz.backend.business.domain.DocumentCalendar;
import com.vz.backend.business.dto.Calendar2ExportDto;
import com.vz.backend.business.dto.CalendarWrapperDto;
import com.vz.backend.business.dto.MeetingCalendarDto;
import com.vz.backend.business.service.Calendar2Service;
import com.vz.backend.business.service.CalendarHistoryService;
import com.vz.backend.business.service.NotificationService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.CalendarActionEnum;
import com.vz.backend.core.config.CalendarStatusEnum;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.config.NotificationHandleStatusEnum;
import com.vz.backend.core.controller.BaseController;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.core.service.IService;
import com.vz.backend.core.service.OrganizationService;
import com.vz.backend.core.service.RoleService;
import com.vz.backend.util.DateTimeUtils;
import com.vz.backend.util.StringUtils;

@RestController
@RequestMapping("/calendar2")
public class Calendar2Controller extends BaseController<Calendar2> {

	enum SortBy {
		UPDATEDATE("updateDate"), // Ngày cập nhật
		CREATEDATE("createDate"), // Ngày tạo
		TITLE("title"), ADDRESS("address"), DESCRIPTION("description"), START_TIME("startTime"), END_TIME("endTime"),
		STATUS("status");

		private String field;

		private SortBy(String field) {
			this.field = field;
		}

		public static String getEnum(String name) {
			for (SortBy v : values()) {
				if (v.name().equals(name)) {
					return v.field;
				}
			}
			return SortBy.CREATEDATE.field;
		}
	}

	@Override
	public IService<Calendar2> getService() {
		return null;
	}

	@Autowired
	private Calendar2Service calendar2Service;

	@Autowired
	private CalendarCommentService calendarCommentService;

	@Autowired
	private RoleService rService;

	@Autowired
	private OrganizationService orgService;
	
	@Autowired
	private NotificationService notiService;
	
	@Autowired
	CalendarHistoryService calendarHistoryservice;
	
	@Autowired
	private FilesStorageService storageService;

	@Autowired
	ICalendar2Repository calendar2Repository;

	@PostMapping(value = "/addCalendar2/{orgType}") //1-Ban /2-CucVuVien /3-Phong
	public ResponseEntity<Calendar2> addCalendar2(@PathVariable Long orgType, @RequestBody Calendar2 calendar) {
		User user = BussinessCommon.getUser();

		if(!calendar.isMeetingCalendar()) { // lịch họp
			if (!rService.isAllowModule(user, ModuleCodeEnum.CAL_MEETING.getName(), ModuleCodeEnum.CAL_BUSINESS.getName())) {
				throw new RestExceptionHandler(Message.CALENDER_NOT_ALLOW);
			}
			if(user != null && !user.getOrgModel().getName().toLowerCase().equals("phòng tổng hợp - văn phòng ban")){
				if (orgType == 1 && !orgService.isUserOfOrgType(user, Constant.BAN)
						|| orgType != 1 && orgService.isUserOfOrgType(user, Constant.BAN)) {
					throw new RestExceptionHandler(Message.CALENDER_NOT_ALLOW);
				}
			}

		}

		calendar.validCalender();
		Boolean isRegisterBan=calendar.getRegisterBan();

		Calendar2 sCalendar = calendar2Service.addCalendar(calendar,orgType);
//		sCalendar.setRegisterBan(isRegisterBan);
		calendarHistoryservice.save(isRegisterBan, sCalendar, CalendarActionEnum.ADD);
		
		return new ResponseEntity<>(sCalendar, HttpStatus.OK);
	}

	@GetMapping(value = "/findByOrg/{orgId}")
	public ResponseEntity<CalendarWrapperDto> findByOrg(@PathVariable Long orgId,
			@RequestParam(defaultValue = "1") int statusType, // 1- register /2-approve /3-publish
			@RequestParam(defaultValue = "1") int orgType, // 1-Ban /2-CucVuVien /3-Phong
			@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page,
			@RequestParam(defaultValue = "0", required = false) int week,
			@RequestParam(defaultValue = "0", required = false) int year) {
		User user = BussinessCommon.getUser();

		if (!rService.isAllowModule(user, ModuleCodeEnum.CAL_MEETING.getName(), ModuleCodeEnum.CAL_BUSINESS.getName())) {
			throw new RestExceptionHandler(Message.CALENDER_NOT_ALLOW);
		}

		if (!user.getOrg().equals(orgId)) {
			throw new RestExceptionHandler(Message.CALENDER_NOT_ALLOW);
		}

		Date start = DateTimeUtils.getDateByWeek(week, year, DateTimeUtils.TYPE_START_DATE);
		Date end = DateTimeUtils.getDateByWeek(week, year, DateTimeUtils.TYPE_END_DATE);
		
		//Pageable pageable = BussinessCommon.castToPageable(page, Sort.by(direction, SortBy.getEnum(sortBy)), size);
		return new ResponseEntity<>(calendar2Service.findByListCondition(start, end, statusType, orgType,
				calendar2Service.getStatus(statusType), week, year), HttpStatus.OK);
	}

	@GetMapping("/getByMonth/{orgType}") // 1-Ban /2-CucVuVien /3-Phong
	public ResponseEntity<List<Calendar2>> getAllCalendar(@PathVariable Long orgType,
			@RequestParam(required = false, defaultValue = "0") int week,
			@RequestParam(required = false, defaultValue = "0") int year) {
		Date start = DateTimeUtils.getDateByWeek(week, year, DateTimeUtils.TYPE_START_DATE);
		Date end = DateTimeUtils.getDateByWeek(week, year, DateTimeUtils.TYPE_END_DATE);
		return new ResponseEntity<>(calendar2Service.findByMonth(orgType, start, end), HttpStatus.OK);
	}
	
	@GetMapping("/getByWeek/{orgType}") // 1-Ban /2-CucVuVien /3-Phong
	public ResponseEntity<CalendarWrapperDto> getAllCalendar1(@PathVariable Long orgType,
			@RequestParam(required = false, defaultValue = "0") int year,
			@RequestParam(required = false, defaultValue = "0") int week) {
		Date start = DateTimeUtils.getDateByWeek(week, year, DateTimeUtils.TYPE_START_DATE);
		Date end = DateTimeUtils.getDateByWeek(week, year, DateTimeUtils.TYPE_END_DATE);
		return new ResponseEntity<>(calendar2Service.getByWeek(orgType, start, end, week, year), HttpStatus.OK);
	}

	/**
	 * for approve calendar
	 * @param id
	 * @param status
	 * @param comment
	 * @return
	 */
	@PostMapping(value = "/updateCalendar/{id}")
	public ResponseEntity<Calendar2> updateStatusCalendar(@PathVariable Long id, @RequestParam CalendarStatusEnum status,
			@RequestParam(required = false) String comment) {

		return new ResponseEntity<>(calendar2Service.updateCalendar(id, status, comment), HttpStatus.OK);
	}

	@PostMapping(value = "/updateCalendarBody/{id}")
	public ResponseEntity<Calendar2> updateBodyCalendar(@PathVariable Long id, @RequestBody Calendar2 news) {

		return new ResponseEntity<>(calendar2Service.updateBodyCalendar(id, news), HttpStatus.OK);
	}
	
	@GetMapping("/getCalendar/{calenderId}")
	public ResponseEntity<Calendar2> getCalendar(@PathVariable Long calenderId) {
		return new ResponseEntity<>(calendar2Service.getCalendar2(calenderId), HttpStatus.OK);
	}

	@GetMapping("/getCalendar2toDate")
	public ResponseEntity<?> getCalendar2toDate(@RequestParam String date, @RequestParam CalendarStatusEnum status, @RequestParam Long roomId) {
		return new ResponseEntity<>(calendar2Service.getCalendar2toDate(date, status, roomId), HttpStatus.OK);
	}

	@GetMapping("/getMeetingCalendar/{meetingId}")
	public ResponseEntity<Calendar2> getMeetingCalendar(@PathVariable Long meetingId) {
		return new ResponseEntity<>(calendar2Service.getByMeetingId(meetingId), HttpStatus.OK);
	}

	@PostMapping("/export")
	public ResponseEntity<StreamingResponseBody> export(@RequestBody Calendar2ExportDto dto) {
		if (dto.getOrgType() == null) {
			throw new RestExceptionHandler("orgType must be number");
		}
		StreamingResponseBody stream = outputStream -> {
			try {
				calendar2Service.export(outputStream, dto);
			} finally {
				StreamUtils.closeOutputStream(outputStream);
			}
		};
		return new ResponseEntity<>(stream, HttpStatus.OK);
	}
	
	@PostMapping("/del/{calendarId}")
	public ResponseEntity<Boolean> del(@PathVariable Long calendarId) {
		return new ResponseEntity<>(calendar2Service.del(calendarId), HttpStatus.OK);
	}
	
	@GetMapping(value = "/findMeetingCalendar")
	public ResponseEntity<CalendarWrapperDto> findMeetingCalendar(
			@RequestParam(defaultValue = "1") int statusType, // 1- register /2-approve /3-publish
			@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page,
			@RequestParam(defaultValue = "0", required = false) int week,
			@RequestParam(defaultValue = "0", required = false) int year,
			@RequestParam(required = false) Long roomId,
			@RequestParam(required = false) Long userId) {
		User user = BussinessCommon.getUser();

		if (!rService.isAllowModule(user, ModuleCodeEnum.CAL_MEETING.getName())) {
			throw new RestExceptionHandler(Message.CALENDER_NOT_ALLOW);
		}

		Date start = DateTimeUtils.getDateByWeek(week, year, DateTimeUtils.TYPE_START_DATE);
		Date end = DateTimeUtils.getDateByWeek(week, year, DateTimeUtils.TYPE_END_DATE);
		
		return new ResponseEntity<>(calendar2Service.findMeetingCalendar(start, end, week, year, roomId, userId), HttpStatus.OK);
	}
	
	@PostMapping("/meeting/update/{id}")
	public ResponseEntity<?> updateMeetingCalendar(@PathVariable Long id, @RequestBody MeetingCalendarDto dto) {
		return new ResponseEntity<>(calendar2Service.updateMeetingCalendar(id, dto), HttpStatus.OK);
	}
	
	@PostMapping("/meeting/attachment/{meetingId}")
	public ResponseEntity<?> uploadMeetingAttachment(@RequestParam MultipartFile[] files, @PathVariable Long meetingId) {
		return new ResponseEntity<>(calendar2Service.addListAttachment(files, meetingId), HttpStatus.OK);
	}
	
	@PostMapping(value = "/meeting/attachment/deleteBy/{id}/{meetingId}")
	public ResponseEntity<?> deleteAttachmentById(@PathVariable Long id, @PathVariable Long meetingId) {
		return new ResponseEntity<>(calendar2Service.deleteMeetingCalendarAttachmentById(id, meetingId),HttpStatus.OK);
	}
	
	/**
	 * Tải file đính kèm
	 * @param fileName
	 * @return
	 */
	@GetMapping("/download/{fileName:.+}")
	public ResponseEntity<Resource> download(@PathVariable String fileName) {
		String name = StringUtils.decodeFromUrl(fileName);
		Resource file = storageService.load(name);
		if (file == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}
		return ResponseEntity.ok()
				.header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + file.getFilename() + "\"")
				.body(file);
	}
	
	@PostMapping(value = "/meeting/deleteBy/{id}")
	public ResponseEntity<?> deleteById(@PathVariable Long id) {
		return new ResponseEntity<>(calendar2Service.deleteMeetingCalendarById(id),HttpStatus.OK);
	}

	@GetMapping(value = "/ban/create")
	public ResponseEntity<?> deleteById() {
		Map<String, Boolean> map = new HashMap<>();
		map.put("isPermission", calendar2Service.createCalendarBan());
		return new ResponseEntity<>(map, HttpStatus.OK);
	}

	@PostMapping(value = "/meeting/atm/add")
	public ResponseEntity<List<AttachmentMeeting>> addAtm(@RequestParam MultipartFile[] files, @RequestParam FileTypeEnum type, @RequestParam Long objId) {
		return new ResponseEntity<>(calendar2Service.saveFileMeeting(files, type, objId), HttpStatus.OK);
	}

	@PostMapping(value = "/meeting/atm/del")
	public ResponseEntity<Boolean> delAtm(@RequestParam String[] names,
										  @RequestParam(defaultValue = "AGENDA") FileTypeEnum type) {
		return new ResponseEntity<>(calendar2Service.delFileMeeting(names, type), HttpStatus.OK);
	}

	@GetMapping(value = "/meeting-person")
	public ResponseEntity<?> getMeetingPerson() {
		int week = Calendar.getInstance().get(Calendar.WEEK_OF_YEAR);
		int year = Calendar.getInstance().get(Calendar.YEAR);
		Date start = new Date();
		if (start.getDay() == 0) {
			start.setDate(start.getDate() + 1);
			week++;
		}
		Date startDate = DateTimeUtils.getDateNotTime(start);
		Date endDate = DateTimeUtils.getDateByWeek(week, year, DateTimeUtils.TYPE_END_DATE);
		return new ResponseEntity<>(calendar2Service.getMeetingPerson(startDate, endDate, week, year), HttpStatus.OK);
	}
}
