package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.Notification;
import com.vz.backend.business.dto.ListNotificationDto;
import com.vz.backend.business.dto.NotificationDto;
import com.vz.backend.business.dto.RoleModuleDto;
import com.vz.backend.business.repository.INotificationRepository;
import com.vz.backend.business.util.sms.ISmsService;
import com.vz.backend.business.util.sms.SmsQueue;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.config.NotificationHandleStatusEnum;
import com.vz.backend.core.domain.Module;
import com.vz.backend.core.domain.Role;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.MailNotiDto;
import com.vz.backend.core.dto.UserFullNamePositionName;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.exception.RestForbidden;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.MailNotiService;
import com.vz.backend.core.service.ModuleService;
import com.vz.backend.core.service.RoleService;
import com.vz.backend.core.service.UserService;

import lombok.Getter;
import org.springframework.transaction.annotation.Transactional;

@Service
public class NotificationService extends BaseService<Notification> implements ISmsService {

	@Autowired
	INotificationRepository notiRepo;

	@Autowired
	UserService userService;

	@Autowired
	ModuleService moduleService;

	@Autowired
	RoleService roleService;

	@Autowired
	MailNotiService mailNotiService;

	@Getter
	@Value("${configs.sms-port:}")
	private String comPort;
	
	@Value("${configs.send-mail: false}")
	private boolean sendMail;

	@Override
	public String getPort() {
		return this.comPort;
	}

	@Override
	public IRepository<Notification> getRepository() {
		return notiRepo;
	}

	public Notification smsAndSave(Notification notification) {
		onNewNotification(notification);
		return super.save(notification);
	}

	public List<Notification> smsAndSaveAll(List<Notification> notifications) {
		notifications
		.forEach(this::onNewNotification);
		return super.saveAll(notifications);
	}

	public Notification add(Long userId, Long docId, String preview, DocumentTypeEnum docType,
			NotificationHandleStatusEnum docOutStatus, ModuleCodeEnum code) {
		Notification input = new Notification();
		input.setUserId(userId);
		input.setDocId(docId);
		input.setDescription(preview);
		input.setDocType(docType);
		input.setDocStatus(docOutStatus);
		input.setModuleCode(code);
		return this.smsAndSave(input);
	}

	public Notification add2(Long userId, Long docId, String preview, DocumentTypeEnum docType,
			NotificationHandleStatusEnum docOutStatus, ModuleCodeEnum code, Long clientId) {
		Notification input = new Notification();
		input.setClientId(clientId);
		input.setUserId(userId);
		input.setDocId(docId);
		input.setDescription(preview);
		input.setDocType(docType);
		input.setDocStatus(docOutStatus);
		input.setModuleCode(code);
		return this.smsAndSave(input);
	}

	public Notification add(Long userId, Long docId, String preview, DocumentTypeEnum docType,
			NotificationHandleStatusEnum docOutStatus, ModuleCodeEnum code, Long actor) {
		Notification input = add(userId, docId, preview, docType, docOutStatus, code);
		input.setActorId(actor);
		return this.smsAndSave(input);
	}

	public List<Notification> addAll(Collection<? extends Long> listUserIds, Long docId, String preview,
			DocumentTypeEnum docType, NotificationHandleStatusEnum docOutStatus, ModuleCodeEnum code) {
		List<Notification> listNoti = new ArrayList<>();
		if (listUserIds == null || listUserIds.isEmpty())
			return listNoti;

		for (Long userId : listUserIds) {
			Notification input = new Notification();
			input.setUserId(userId);
			input.setDocId(docId);
			input.setDescription(preview);
			input.setDocType(docType);
			input.setDocStatus(docOutStatus);
			if (NotificationHandleStatusEnum.DA_THU_HOI_BH.equals(docOutStatus)) {
				input.setDescription(preview);
			}
			input.setModuleCode(code);
			listNoti.add(input);
		}
		return this.smsAndSaveAll(listNoti);
	}

	public List<Notification> addAll(List<Long> listUserIds, Long docId, String preview, DocumentTypeEnum docType,
			NotificationHandleStatusEnum docOutStatus, ModuleCodeEnum code, Long actorId) {
		List<Notification> listNoti = addAll(listUserIds, docId, preview, docType, docOutStatus, code);
		listNoti.forEach(i-> {
			i.setActorId(actorId);
		});
		return this.smsAndSaveAll(listNoti);
	}

	public ListNotificationDto get(Long userId) {
		List<Notification> listNoti = notiRepo.getByActiveAndUserId(true, userId);
		ListNotificationDto result = new ListNotificationDto();
		List<NotificationDto> listNotiDto = new ArrayList<>();
		for (Notification noti : listNoti) {
			NotificationDto notiDto = new NotificationDto();
			notiDto.setId(noti.getId());
			notiDto.setDocId(noti.getDocId());
			notiDto.setDocType(noti.getDocType().getName());
			notiDto.setDescription(noti.getDescription());
			notiDto.setDocStatus(noti.getDocStatus());
			notiDto.setDocStatusName(noti.getDocStatus().getName());
			notiDto.setRead(noti.getRead());
			notiDto.setDate(noti.getCreateDate());
			notiDto.setModuleCode(noti.getModuleCode());
			notiDto.setActor(noti.getActor() == null ? "" : noti.getActor().getFullName());
			listNotiDto.add(notiDto);
		}
		result.setTotalRecord(listNotiDto.size());
		result.setTotalUnread(notiRepo.countByActiveAndUserIdAndRead(true, userId, false));
		result.setObjList(listNotiDto);
		return result;
	}
	public Long getTotal(Long userId) {
		return notiRepo.countByActiveAndUserIdAndRead(true, userId, false);
	}
	private void onNewNotification(Notification notification) {
		if (!this.sendMail)
			return;
		SmsQueue.addQueue(notification.getUserId(), notification.content(), this);
		mailNotiService.sendNotiHasException(toMailNotiDto(notification));
	}

	public Boolean delete(Long id) {
		Optional<Notification> dc = notiRepo.findById(id);
		if (!dc.isPresent()) {
			throw new RestExceptionHandler(Message.ACTION_FAILED);
		}
		Notification noti = dc.get();
		if (noti.getUserId().equals(BussinessCommon.getUserId())) {
			noti.setActive(false);
			notiRepo.save(noti);
		}
		return true;
	}

	public void deleteSchedule(Calendar c) {
		Date now = c.getTime();
		c.set(Calendar.MINUTE, 0);
		Date last = c.getTime();
		notiRepo.deleteByCreateDateAndModuleCode(last, now, ModuleCodeEnum.SCHEDULE_REMIND);
	}

	public Long countUnread(Long userId) {
		return notiRepo.countByActiveAndUserIdAndRead(true, userId, false);
	}

	@Transactional
	public Boolean deactiveAllByUserId(Long userId) {
		notiRepo.deactiveAllByUserId(userId);
		return true;
	}

	public void deactiveAllByDocIdAndDocType(Long docId, DocumentTypeEnum docType) {
		List<Notification> list = notiRepo.deactiveAllByDocIdAndDocType(docId, docType);
		for (Notification noti: list) {
			noti.setActive(false);
		}
		notiRepo.saveAll(list);
	}

	public Boolean deactiveAllByIds(Long[] listIds) {
		notiRepo.deactiveAllByIds(listIds);
		return true;
	}
	
	@Transactional
	public Boolean setRead(Long id) {
		notiRepo.setReadByIdAndUserId(true, id, BussinessCommon.getUserId());
		return true;
	}

	public void setActiveByDocIdAndDocType(Long docId, DocumentTypeEnum docType, Boolean active) {
		notiRepo.setActiveByDocIdAndDocType(docId, docType, active);
	}

	public void setActiveByListUserIdAndDocIdAndDocType(List<Long> listUsers, Long docId, DocumentTypeEnum docType,
			boolean active) {
		notiRepo.setActiveByListUserIdAndDocIdAndDocType(listUsers, docId, docType, active);
	}

	public void setActiveByUserIdAndDocIdAndDocType(Long userId, Long docId, DocumentTypeEnum docType, boolean active) {
		notiRepo.setActiveByUserIdAndDocIdAndDocType(userId, docId, docType, active);
	}

	public void setActiveForUserHandle(Long userId, Long docId, DocumentTypeEnum docType, boolean active,
			NotificationHandleStatusEnum docStatus, ModuleCodeEnum module) {
		List<Notification> rs = notiRepo.getByUserId(userId, docId, docType, true, docStatus, module);
		rs.forEach(i -> {
			i.setActive(active);
		});
		if (!rs.isEmpty()) {
			notiRepo.saveAll(rs);
		}
	}

	public RoleModuleDto checkModule(long notiId) {
		RoleModuleDto rmDto = new RoleModuleDto();
		Optional<Notification> oNoti = notiRepo.findById(notiId);
		if (oNoti.isPresent()) {
			Notification noti = oNoti.get();
			noti.setRead(true);
			notiRepo.save(noti);
			if (noti.getModuleCode() == null) {
				return rmDto; // Thoải mái đi
			}
			User user = BussinessCommon.getUser();
			// Check if current role have module -> return;
			if (moduleService.existModuleByRoleId(noti.getModuleCode(), user.getCurrentRole())) {
				return rmDto; // Ok luôn
			}
			// Find role by user have module
			List<Role> listRole = roleService.getRoleHaveModuleByUser(noti.getModuleCode(), user);
			if (listRole == null || listRole.isEmpty()) {
				noti.setActive(false);
				notiRepo.save(noti);

				String prefix = "";
				Module m = moduleService.findByCode(noti.getModuleCode().getName());
				if (m == null) {
					prefix = noti.getModuleCode().getName();
				} else {
					prefix = m.getName();
					Optional<Module> pOption = moduleService.findById(m.getParentId());
					if (pOption.isPresent()) {
						prefix = pOption.get().getName() + " > " + prefix;
					}
				}

				throw new RestForbidden("Bạn không có quyền truy cập module " + prefix); // Ko có role hợp lý
			}
			// Switch role
			long roleId = listRole.get(0).getId();
			rmDto.setRoleId(roleId);
			rmDto.setListModule(moduleService.findByRoleIdList(Arrays.asList(roleId), null));
			userService.setCurrentRole(BussinessCommon.getUserId(), roleId);
			return rmDto;
		}
		throw new RestExceptionHandler(Message.NOTIFICATION_NOT_FOUND);
	}

	public void setActiveByDocIdAndDocTypeAndUserIdAndDocStatus(boolean active, Long docId, DocumentTypeEnum docType,
			Long userId, List<NotificationHandleStatusEnum> listStatus) {
		notiRepo.setActiveByDocIdAndDocTypeAndUserIdAndDocStatus(active, docId, docType, userId, listStatus);
	}

	public void setReadByDocIdAndDocTypeAndUserId(Boolean read, Long docId, DocumentTypeEnum docType, Long userId) {
		List<Notification> listNotification = notiRepo.setReadByDocIdAndDocTypeAndUserId(docId, docType, userId);
		List<Notification> listResult = new ArrayList<>();
		for (Notification notification: listNotification) {
			notification.setRead(read);
			listResult.add(notification);
		}
		notiRepo.saveAll(listNotification);
	}

	public void setReadById(Long id) {
		Notification n = notiRepo.findByClientIdAndId(BussinessCommon.getClientId(), id);
		if (n != null) {
			n.setRead(true);
		}
		notiRepo.save(n);
	}

	@Override
	public String getPhone(Long userId) {
		return userService.getPhone(userId);
	}

	public MailNotiDto toMailNotiDto(Notification notification) {
		UserFullNamePositionName dto = userService.fullNamePositionName(notification.getUserId());
		return MailNotiDto.builder()
				.preview(notification.getFullDescription())
				.position(dto.getPositionName())
				.fullName(dto.getFullname())
				.docType(notification.getDocType().getName())
				.email(dto.getEmail())
				.build();
	}
}
