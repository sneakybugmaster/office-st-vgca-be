package com.vz.backend.business.service;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Timestamp;
import java.util.*;
import java.util.Calendar;
import java.util.stream.Collectors;

import com.vz.backend.business.config.ecabinet.MeetingStatusEnum;
import com.vz.backend.business.domain.*;
import com.vz.backend.business.dto.*;
import com.vz.backend.core.config.*;
import com.vz.backend.core.dto.UserBasicDto;
import com.vz.backend.core.repository.*;
import com.vz.backend.core.service.*;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;
import org.wickedsource.docxstamper.DocxStamper;
import org.wickedsource.docxstamper.DocxStamperConfiguration;
import org.wickedsource.docxstamper.replace.typeresolver.image.Image;

import com.vz.backend.business.config.ReceiveTypeEnum;
import com.vz.backend.business.config.ecabinet.FileTypeEnum;
import com.vz.backend.business.config.ecabinet.RoleEnum;
import com.vz.backend.business.domain.ecabinet.Agenda;
import com.vz.backend.business.domain.ecabinet.AttachmentMeeting;
import com.vz.backend.business.domain.ecabinet.DocumentMeeting;
import com.vz.backend.business.domain.ecabinet.Meeting;
import com.vz.backend.business.domain.ecabinet.Participant;
import com.vz.backend.business.dto.calendar.Calendar2Context;
import com.vz.backend.business.dto.calendar.Calendar2Part;
import com.vz.backend.business.repository.ICalendar2IngredientRepository;
import com.vz.backend.business.repository.ICalendar2Repository;
import com.vz.backend.business.repository.ICalendarJoin2Repository;
import com.vz.backend.business.repository.ecabinet.IAgendaRepository;
import com.vz.backend.business.repository.ecabinet.IAttachmentMeetingReposiroty;
import com.vz.backend.business.repository.ecabinet.IMeetingRepository;
import com.vz.backend.business.repository.ecabinet.IParticipantRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.GroupUser;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.Calendar2OrgInfo;
import com.vz.backend.core.dto.OrgGroupDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.util.DateTimeUtils;
import com.vz.backend.util.StringUtils;

@Service
public class Calendar2Service extends BaseService<Calendar2> {
    private Long tmpOrgId = null;

    @Autowired
    ICalendar2Repository calendar2Repository;

    @Autowired
    CalendarCommentService calendarCommentService;

    @Autowired
    CalendarHistoryService calendarHistoryservice;

    @Autowired
    ICalendarJoin2Repository calendarJoin2Reposiroty;

    @Autowired
    CalendarJoin2Service calendarJoinService;

    @Autowired
    OrganizationService orgService;

    @Autowired
    OrganizationRepository orgRepo;

    @Autowired
    FilesStorageService storageService;

    @Autowired
    AuthorityUserService authorityService;

    @Autowired
    private NotificationService notiService;

    @Autowired
    UserService userService;

    @Autowired
    AttachmentCalendarService attService;

    @Autowired
    private ICalendar2IngredientRepository calendar2IngredientRepository;

    @Autowired
    private IMeetingRepository meetingRepository;

    @Autowired
    private IUserRepository userRepository;

    @Autowired
    private IAttachmentMeetingReposiroty atmRepository;

    @Autowired
    private IGroupRepository groupRepository;

    @Autowired
    private IParticipantRepository participantRepository;

    @Autowired
    private IGroupUserRepository groupUserRepository;
    @Autowired
    private IAgendaRepository agendaRepository;

    @Autowired
    private EncryptionService encryptService;

    @Autowired
    private RoleService roleService;

    @Autowired
    private IAuthorityUserRepository iAuthorityUserRepository;

    private final Path root = Paths.get("uploads");

    @Override
    public IRepository<Calendar2> getRepository() {
        return calendar2Repository;
    }

    public Calendar2 addCalendar(Calendar2 calendar, Long isType) {
        checkFullNameNotFound(calendar.getParticipants());
        User user = BussinessCommon.getUser();
        List<DocumentCalendar> docInList = calendar.getDInList();
        List<DocumentCalendar> docOutList = calendar.getDOutList();
        List<DocumentCalendar> taskList = calendar.getTaskList();

        if (calendar.isMeetingCalendar()) { // lịch họp

            calendar.validMeetingCalendar();

            //valid time + room duplicate
            if (isRoomTimeDuplicate(calendar.getRoomId(), calendar.getStartTime(), calendar.getEndTime())) {
                throw new RestExceptionHandler(Message.ROOM_TIME_DUPLICATE);
            }

            calendar.setUnitCalendar(false);
            calendar.setOrgId(user.getOrg());
            calendar.setMeetingCalendar(true);
            calendar = calendar2Repository.save(calendar);
            saveIngredient(calendar, false);
            return saveDocRelated(calendar, docInList, docOutList, taskList);
        }
        if (isType != null) {
            calendar.setUnitCalendar(isType == 1 ? false : true);
        } else {
            calendar.setUnitCalendar(!orgService.isUserOfOrgType(user, Constant.BAN));
        }
        calendar.setOrgId(user.getOrg());
        calendar.setMeetingCalendar(false);
        calendar.clear();
        calendar = calendar2Repository.save(calendar); //lịch của cục
        saveDocRelated(calendar, docInList, docOutList, taskList);
        addNotiRequestApproveCalendar(calendar);

        // đăng kí lên Ban -> tạo thêm lịch của Ban
        if (Boolean.TRUE.equals(calendar.getRegisterBan())) {
            calendar.setRegisterBan(false);
            Calendar2 calendarForBan = new Calendar2();
            calendarForBan.setCalendater(false, calendar);
            calendarForBan.setMeetingCalendar(false);
            calendarForBan.setOrgId(orgService.getRootOrgId(user.getOrg()));
            calendarForBan = calendar2Repository.save(calendarForBan);
            saveDocRelated(calendarForBan, docInList, docOutList, taskList);
            addNotiRequestApproveCalendar(calendar);
        }
        saveIngredient(calendar, false);
        return calendar;
    }

    public Calendar2 saveDocRelated(Calendar2 c, List<DocumentCalendar> dInList, List<DocumentCalendar> dOutList, List<DocumentCalendar> taskList) {
        List<DocumentCalendar> nInList = new ArrayList<>();
        List<DocumentCalendar> nOutList = new ArrayList<>();
        List<DocumentCalendar> tList = new ArrayList<>();
        dInList.forEach(i -> nInList.add(new DocumentCalendar(c, i.getDocInId(), DocumentTypeEnum.VAN_BAN_DEN)));
        dOutList.forEach(i -> nOutList.add(new DocumentCalendar(c, i.getDocOutId(), DocumentTypeEnum.VAN_BAN_DI)));
        taskList.forEach(i -> tList.add(new DocumentCalendar(c, i.getTaskId(), DocumentTypeEnum.GIAO_VIEC)));
        c.getDInList().clear();
        c.getDOutList().clear();
        c.getTaskList().clear();
        c.setDInList(nInList, c.getId());
        c.setDOutList(nOutList, c.getId());
        c.setTaskList(tList, c.getId());
        try {
            return calendar2Repository.save(c);
        } catch (Exception e) {
            throw new RestExceptionHandler(Message.CALENDAR_DATA_INVALD);
        }
    }

    public Meeting saveDocMeetingRelated(Meeting m, List<DocumentCalendar> dInList, List<DocumentCalendar> dOutList) {
        List<DocumentMeeting> nInList = new ArrayList<>();
        List<DocumentMeeting> nOutList = new ArrayList<>();

        dInList.forEach(i -> nInList.add(new DocumentMeeting(m, i.getDocInId(), DocumentTypeEnum.VAN_BAN_DEN)));
        dOutList.forEach(i -> nOutList.add(new DocumentMeeting(m, i.getDocOutId(), DocumentTypeEnum.VAN_BAN_DI)));
        m.getDInList().clear();
        m.getDOutList().clear();

        m.setDInList(nInList, m.getId());
        m.setDOutList(nOutList, m.getId());

        try {
            return meetingRepository.save(m);
        } catch (Exception e) {
            throw new RestExceptionHandler(Message.INVALID_INPUT_DATA);
        }
    }

    public void addNotiRequestApproveCalendar(Calendar2 calendar) {
        NotificationHandleStatusEnum notiEnum = NotificationHandleStatusEnum.CAL_YC_DUYET_CVV;
        if (Boolean.TRUE.equals(calendar.getRegisterBan()))
            notiEnum = NotificationHandleStatusEnum.CAL_YC_DUYET_BAN;
        if (calendar.getOrgId() == null) return;
        Organization orgCreate;
        if (calendar.getOrgModel() == null) {
            Optional<Organization> t = orgService.findById(calendar.getOrgId());
            if (!t.isPresent()) return;
            orgCreate = t.get();
        } else {
            orgCreate = calendar.getOrgModel();
        }
        List<Long> listUser;
        if (Boolean.TRUE.equals(calendar.getRegisterBan())) {
            Organization org = orgService.findParentOrgByTypeAndOrgId(Constant.BAN, orgCreate);
            if (org == null) return;
            listUser = authorityService.getListUserApprovalByOrgIdAndAuthority(org.getId(), AuthorityEnum.APPROVE_TOP_LEVEL_CALENDAR);
        } else {
            Organization org = orgService.findParentOrgByTypeAndOrgId(Constant.CUC_VU_VIEN, orgCreate);
            if (org == null) return;
            listUser = authorityService.getListUserApprovalByOrgIdAndAuthority(org.getId(), AuthorityEnum.APPROVE_UNIT_LEVEL_CALENDAR);
        }
        notiService.addAll(listUser, calendar.getId(), calendar.getTitle(), DocumentTypeEnum.TAO_LICH, notiEnum, calendar.isMeetingCalendar() ? ModuleCodeEnum.CAL_MEETING : ModuleCodeEnum.CAL_BUSINESS);
    }

    // statusType, //1-register /2-approve /3-publish
    // orgType, //1-Ban /2-CucVuVien /3-Phong
    public Page<Calendar2> findByCondition(Date start, Date end, int statusType, int orgType,
                                           List<CalendarStatusEnum> status, Pageable pageable) {
        User user = BussinessCommon.getUser();
        List<Long> orgIds;
        Long org = user.getOrg();
        Page<Calendar2> rsPage;
        Organization orgParents = null;
        if (!orgService.isUserOfOrgType(user, Constant.BAN) && !orgService.isUserOfOrgType(user, Constant.CUC_VU_VIEN)) {
            org = orgService.getParentByOrgType(user.getOrgModel(), Constant.CUC_VU_VIEN);
        }

        if (org == null) {
            org = orgService.getParentByOrgType(user.getOrgModel(), Constant.BAN);
        }

        orgParents = orgService.findByClientIdAndId(org);
        orgIds = orgService.orgAndSub(org);

        if (statusType == Constant.CALENDAR_SCREEN_REGISTER) { //for register screen
            if (orgType == Constant.CALENDAR_ORG_TYPE_CUC_VU_VIEN) {
                if (orgService.isUserOfOrgType(user, Constant.BAN))
                    return new PageImpl<>(new ArrayList<>(), pageable, 0L);

                rsPage = calendar2Repository.findByUnitLevel(false, start, end, orgIds, status, user.getClientId(),
                        pageable);

            } else {

                if (orgService.isUserOfOrgType(user, Constant.BAN)) {
                    rsPage = calendar2Repository.findByTopLevel(true, false, start, end, orgIds, status, user.getClientId(),
                            pageable);
                } else {
                    rsPage = calendar2Repository.findByUnitLevel(true, start, end, orgIds, status, user.getClientId(),
                            pageable);
                }

            }
        } else { //for approve screen

            if (orgService.isUserOfOrgType(user, Constant.BAN)) {
                rsPage = calendar2Repository.findByTopLevel(true, false, start, end, orgIds, status, user.getClientId(),
                        pageable);
            } else {
                rsPage = calendar2Repository.findByTopLevel(true, true, start, end, orgIds, status, user.getClientId(),
                        pageable);
            }
        }

        return setPage(orgParents, rsPage, pageable);
    }

    private Page<Calendar2> setPage(Organization orgParents, Page<Calendar2> page, Pageable pageable) {
        User user = BussinessCommon.getUser();
        if (BussinessCommon.isEmptyPage(page)) {
            return new PageImpl<>(new ArrayList<>(), pageable, 0L);
        }

        for (Calendar2 i : page.getContent()) {

            boolean isUserCanApprove = checkUserCanApprove(user, i);
            i.setShowApproveBt(canApprove(i, false, isUserCanApprove));
            i.setShowRejectBt(canApprove(i, true, isUserCanApprove));
            i.setParentOrgName(setParentOrgName(user, i.getOrgModel(), orgParents));
            i.setShowEditBt(canEdit(user, i, isUserCanApprove));
            i.setShowCancelBt(canCancel(i, isUserCanApprove));
            i.setShowDelBt(canDel(i, isUserCanApprove));
        }

        setCreateByUser(page.getContent());

        return page;
    }

    private List<Calendar2> setCreateByUser(List<Calendar2> all) {
        if (!BussinessCommon.isEmptyList(all)) {
            List<Long> createByIds = all.stream().map(Calendar2::getCreateBy).distinct().collect(Collectors.toList());
            List<User> createByUsers = userService.findByIds(createByIds, true);
            for (Calendar2 i : all) {
                Optional<User> u = createByUsers.stream().filter(in -> in.getId().equals(i.getCreateBy())).findFirst();
                i.setCreateUserName(u.isPresent() ? u.get().getFullName() : "");
            }
        }
        return all;
    }

    private String setParentOrgName(User user, Organization org, Organization orgParents) {
        if (orgService.isUserOfOrgType(user, Constant.BAN)) {
            Long orgParentId = orgService.getParentByOrgType(org, Constant.CUC_VU_VIEN);
            Organization orgParentObj = orgService.findByClientIdAndId(orgParentId);
            return orgParentObj != null ? orgParentObj.getName() : null;
        } else if (orgParents != null && !orgParents.getId().equals(org.getId())) {
            return orgParents.getName();
        }
        return null;
    }

    public List<Calendar2> findByMonth(Long orgType, Date startDate, Date endDate) {
        User u = BussinessCommon.getUser();
        Long org = u.getOrg();
        List<Long> orgIds = new ArrayList<>();
        Sort sort = Sort.by("startTime");
        boolean isBan;
        if (orgType == Constant.CALENDAR_ORG_TYPE_BAN) { // for BAN
            isBan = true;
            if (!orgService.isUserOfOrgType(u, Constant.BAN)) {
                org = orgService.getParentByOrgType(u.getOrgModel(), Constant.BAN);
            }
            orgIds = orgService.orgAndSub(org);
            if (!orgIds.isEmpty() && orgIds.contains(org)) {
                orgIds.remove(org);
            }
        } else {
            isBan = false;
            if (!orgService.isUserOfOrgType(u, Constant.CUC_VU_VIEN)) {
                org = orgService.getParentByOrgType(u.getOrgModel(), Constant.CUC_VU_VIEN);
            }
            if (org == null) {
                orgIds.add(u.getOrg());
            } else {
                orgIds = orgService.orgAndSub(org);
            }

        }

        tmpOrgId = org;
        if (isBan) {
            return calendar2Repository.findByMonthOfBan(startDate, endDate, u.getClientId(), true, u.getId(), iAuthorityUserRepository.checkAuthorByPermission(AuthorityEnum.APPROVE_TOP_LEVEL_CALENDAR, u.getClientId(), u.getId()), sort);
        }
        List<Calendar2> calendar2List = calendar2Repository.findByMonthOfCuc(startDate, endDate, orgIds, u.getClientId(), true, u.getId(), iAuthorityUserRepository.checkAuthorByPermission(AuthorityEnum.APPROVE_UNIT_LEVEL_CALENDAR, u.getClientId(), u.getId()), sort);

        return calendar2List;
    }

    public Calendar2 findByIdAndClientIdAndActive(Long id, boolean active) {
        return calendar2Repository.findByIdAndClientIdAndActive(id, BussinessCommon.getClientId(), active);
    }

    public List<Calendar2> findIsInvitation(Integer month, Integer year, Long userId, Long clientId) {
        int m = 1;
        int y = 100;
        if (month == null) {
            m = 0;
            month = 0;
        }

        if (year == null) {
            y = 0;
            year = 0;
        }
        return calendar2Repository.findIsInvitation(month, year, m, y, userId, clientId);
    }

    private Calendar2 validCalendar(Long calendarId) {
        Calendar2 c = findByIdAndClientIdAndActive(calendarId, true);
        if (c == null) {
            throw new RestExceptionHandler(Message.CALENDAR_INVALD);
        }
        return c;
    }

    public Calendar2 getCalendar2(Long calenderId) {
        Calendar2 c = validCalendar(calenderId);
        User user = userService.findByClientIdAndId(BussinessCommon.getClientId(), c.getCreateBy());
        c.setCreateUserName(user != null ? user.getFullName() : "");
        if (user != null) {
            Organization orgCreateBy = user.getOrgModel();

            if (orgCreateBy != null) {
                c.setOrgModel(orgCreateBy);
            }

            Long parentOrgId = user.getOrgModel().getParentId();
            if (parentOrgId != null) {
                Organization parentOrg = orgService.getOne(parentOrgId);
                if (parentOrg != null) {
                    c.setParentOrgName(parentOrg.getName());
                }
            } else {
                c.setParentOrgName("");
            }
        }

        if (!c.isMeetingCalendar()) {
            c.setAttCalWeek(attService.getAttCalWeek(c.getStartTime()));
        }
        c.setParticipantsOrg(calendar2IngredientRepository.getListOrgIngredient(ReceiveTypeEnum.ORG, BussinessCommon.getClientId(), c.getId()));
        c.setParticipantsGroup(calendar2IngredientRepository.getListGroupIngredient(ReceiveTypeEnum.GROUP, BussinessCommon.getClientId(), c.getId()));
        c.setParticipantsPersonal(getListMemberByCalendarId(calenderId, ReceiveTypeEnum.USER));
        c.setParticipantsGuests(getListMemberByCalendarId(calenderId, ReceiveTypeEnum.GUEST));
        c.setAttachments(attService.getByObjId(calenderId, ObjTypeEnum.CALENDAR));
        c.setContentFile(atmRepository.getByClientIdAndObjIdAndFileTypeAndActiveTrue(BussinessCommon.getClientId(), calenderId, FileTypeEnum.AGENDA));
        c.setIsShowAttachments(showAttachmentCalendar(c, BussinessCommon.getUser().getOrg(), c.getCreateBy()));
        return c;
    }

    private Image getImage(String signature) {
        if (StringUtils.isNullOrEmpty(signature)) {
            return null;
        }
        Resource resource = storageService.load(signature);
        if (resource == null) {
            return null;
        }
        String fileName = resource.getFilename();
        if (fileName == null || fileName.length() == 0) {
            return null;
        }

        try {
            return new Image(resource.getInputStream(), 2000);
        } catch (IOException e) {
            return null;
        }
    }

    public void export(OutputStream outputStream, Calendar2ExportDto dto) throws IOException {
        List<Calendar2> info = findByMonth(dto.getOrgType(), dto.getStartDate(), dto.getEndDate());
        Long orgId = tmpOrgId;
        Calendar2OrgInfo orgInfo = orgService.orgInfo(orgId);
        String signature = orgInfo.getConfig().getSignature();
        Image image = null;
        if (!StringUtils.isNullOrEmpty(signature)) {
//			throw new RestExceptionHandler("Chưa có cấu hình chữ kí cho đơn vị " + orgId);
            Image signImage = getImage(signature);
            if (signImage != null) {
                signImage.setAltText("Chữ ký");
                image = signImage;
//			throw new RestExceptionHandler("Không tìm thấy file" + signature);
            }
        }
        Calendar2Context context = new Calendar2Context(dto, orgInfo, image);
        if (dto.getOrgType() == 1) {
            context.setOrgName("");
        }
        String title = "Lịch công tác";
        context.add(info);
        Calendar c = Calendar.getInstance(DateTimeUtils.timeZone());
        Calendar timeIn = Calendar.getInstance();
        timeIn.setTime(dto.getStartDate());
        int week = c.get(Calendar.WEEK_OF_YEAR);
        int weekIn = timeIn.get(Calendar.WEEK_OF_YEAR);
        if (weekIn > week) {
            title = "Lịch dự kiến";
        }
        String[] listThu = {"Thứ hai", "Thứ ba", "Thứ tư", "Thứ năm", "Thứ sáu", "Thứ bảy", "Chủ nhật"};
        for (int i = 0; i < listThu.length; i++) {
            String ngay = listThu[i];
            List<Calendar2Part> listThuExits = context.getCalendar2Part().stream().filter(item -> item.getDateStr().trim().toLowerCase().contains(ngay.toLowerCase())).collect(Collectors.toList());
            if (listThuExits.size() == 0) {
                Calendar2 calendar2 = new Calendar2();
                calendar2.setStartTime(DateTimeUtils.getDayAfter(dto.getStartDate(), i));
                Calendar2Part cPart = new Calendar2Part(calendar2);
                cPart.setTime("");
                context.getCalendar2Part().add(cPart);
            }
        }
        context.setTitle(title.toUpperCase());
        context.getCalendar2Part().sort((a, b) -> a.getDate().compareTo(b.getDate()));
        if (dto.getOrgType() == 2) {
            context.setTl("");
        }
        DocxStamperConfiguration stamperConfig = new DocxStamperConfiguration();
        stamperConfig.setLineBreakPlaceholder(Calendar2Part.BREAK_LINE);
        @SuppressWarnings("unchecked")
        DocxStamper<Calendar2Context> stamper = stamperConfig.build();
        InputStream template = getClass().getClassLoader().getResourceAsStream("calendar_template2.docx");
        if (template != null) {
            stamper.stamp(template, context, outputStream);
        }
        try {
            if (template != null) {
                template.close();
            }
            outputStream.close();
        } catch (IOException e) {
        }
    }

    public boolean checkUserCanApprove(User user, Calendar2 c) {
        if (c.isUnitCalendar() && Boolean.FALSE.equals(c.getRegisterBan())) { // unit level calendar
            return authorityService.isUserHasAuthority(user.getId(), null, AuthorityEnum.APPROVE_UNIT_LEVEL_CALENDAR);
        } else { // top level calendar
            return authorityService.isUserHasAuthority(user.getId(), null, AuthorityEnum.APPROVE_TOP_LEVEL_CALENDAR);
        }
    }

    public boolean canApprove(User user, Calendar2 c) {
        return checkUserCanApprove(user, c);
    }

    private boolean canApprove(Calendar2 c, boolean isShowReject, boolean isUserCanApprove) {
        if (isShowReject) {
            return CalendarStatusEnum.PRE_APPROVE.equals(c.getStatus()) && isUserCanApprove;
        }

        return !CalendarStatusEnum.APPROVE.equals(c.getStatus()) && isUserCanApprove;
    }

    private void setAttachments(List<Calendar2> all) {
        List<Long> allIds = all.stream().map(Calendar2::getId).collect(Collectors.toList());
        List<AttachmentCalendar> atts = attService.getByObjIds(allIds, ObjTypeEnum.CALENDAR);
        long curentOrgId = BussinessCommon.getUser().getOrg();
        for (Calendar2 i : all) {
            List<AttachmentCalendar> sub = new ArrayList<>();
            for (AttachmentCalendar a : atts) {
                if (a.getObjId().equals(i.getId())) {
                    sub.add(a);
                }
            }
            i.setAttachments(sub);
            i.setParticipantsOrg(calendar2IngredientRepository.getListOrgIngredient(ReceiveTypeEnum.ORG, BussinessCommon.getClientId(), i.getId()));
            i.setParticipantsGroup(calendar2IngredientRepository.getListGroupIngredient(ReceiveTypeEnum.GROUP, BussinessCommon.getClientId(), i.getId()));
            i.setIsShowAttachments(showAttachmentCalendar(i, curentOrgId, i.getCreateBy()));
        }
    }

    public boolean showAttachmentCalendar(Calendar2 calendar2, Long currentOrgId, Long userCreateId) {
        Long userCurrent = BussinessCommon.getUserId();
        if (iAuthorityUserRepository.checkAuthorByPermission(AuthorityEnum.APPROVE_UNIT_LEVEL_CALENDAR, BussinessCommon.getClientId(), BussinessCommon.getUserId())) {
            return true;
        } else if (userCreateId.toString().equals(userCurrent.toString())) {
            return true;
        } else {
            for (OrgGroupDto orgGroupDto : calendar2.getParticipantsOrg()) {
                if (currentOrgId == orgGroupDto.getId()) {
                    return true;
                }
            }
            if (calendar2.getParticipants() != null) {
                if (calendar2.getParticipants().toLowerCase().contains(BussinessCommon.getUser().getFullName().toLowerCase())) {
                    return true;
                }
            }
            List<Long> userGroupId = calendar2.getParticipantsGroup().stream().map(OrgGroupDto::getId).collect(Collectors.toList());
            if (userRepository.checkUserInListGroup(BussinessCommon.getClientId(), true, BussinessCommon.getUserId(), userGroupId)) {
                return true;
            }
            return false;
        }
    }

    private void getOrgGroupParticipants(Meeting meeting, Calendar2 c) {
        List<Long> orgIds = meeting.getParticipants().stream().filter(i -> ReceiveTypeEnum.ORG.equals(i.getType()))
                .map(Participant::getObjId).distinct().collect(Collectors.toList());
        List<Long> groupIds = meeting.getParticipants().stream()
                .filter(i -> ReceiveTypeEnum.GROUP.equals(i.getType())).map(Participant::getGroupId).distinct()
                .collect(Collectors.toList());

        List<OrgGroupDto> orgs = orgRepo.findByClientIdAndIdInAndActiveTrue(BussinessCommon.getClientId(), orgIds);
        List<OrgGroupDto> groups = groupRepository.findByClientIdAndIdInAndActiveTrue(BussinessCommon.getClientId(), groupIds);
        c.setParticipantsOrg(orgs);
        c.setParticipantsGroup(groups);
    }

    private List<OrgGroupDto> getListOrgCabinet(Meeting meeting) {
        List<Long> orgIds = meeting.getParticipants().stream().filter(i -> ReceiveTypeEnum.ORG.equals(i.getType())).map(Participant::getObjId).distinct().collect(Collectors.toList());
        return calendar2IngredientRepository.findOrgByListId(BussinessCommon.getClientId(), orgIds);
    }

    private List<OrgGroupDto> getListGroupCabinet(Meeting meeting) {
        List<Long> groupIds = meeting.getParticipants().stream().filter(i -> ReceiveTypeEnum.GROUP.equals(i.getType())).map(Participant::getGroupId).distinct().collect(Collectors.toList());
        return calendar2IngredientRepository.findGroupByListId(BussinessCommon.getClientId(), groupIds);
    }

    private List<Calendar2> findCabinetByMonth(Date start, Date end) {
        List<Calendar2> all = meetingRepository.findMeetingCalendar(BussinessCommon.getClientId(),
                BussinessCommon.getUserId(), start, end);
        for (Calendar2 c : all) {
            Meeting meeting = meetingRepository.findByClientIdAndIdAndActiveTrue(BussinessCommon.getClientId(), c.getId());
            getOrgGroupParticipants(meeting, c);
            c.setParticipants(getUserNameList(meeting));
            c.setAttachments(getMeetingInviation(meeting));
            c.getAttachments().addAll(getMeetingAgendaAttachment(meeting));
            c.setParticipantsOrg(getListOrgCabinet(meeting));
            c.setParticipantsGroup(getListGroupCabinet(meeting));
            c.setCreateBy(meeting.getCreateBy());
        }
        return all;
    }

    private String getUserNameList(Meeting meeting) {
        List<String> fullNames = new ArrayList<>();
        if (meeting != null) {
            for (Participant p : meeting.getParticipants()) {
                ReceiveTypeEnum[] enums = {ReceiveTypeEnum.USER};
                if (Arrays.asList(enums).contains(p.getType())) {
                    User user = userRepository.findByClientIdAndId(BussinessCommon.getClientId(), p.getObjId());
                    if (user != null)
                        fullNames.add(user.getFullName());

                }
            }
        }

        return fullNames.stream().collect(Collectors.joining(", "));
    }

    private List<AttachmentCalendar> getMeetingInviation(Meeting meeting) {
        List<AttachmentCalendar> rs = new ArrayList<>();
        List<AttachmentMeeting> invitation = atmRepository.findByClientIdAndObjIdAndFileType(
                BussinessCommon.getClientId(), meeting.getId(), FileTypeEnum.INVITATION);
        if (!BussinessCommon.isEmptyList(invitation)) {
            rs.add(new AttachmentCalendar(invitation.get(0).getId(), invitation.get(0).getName(), true));
        }
        return rs;
    }

    private List<AttachmentCalendar> getMeetingAgendaAttachment(Meeting meeting) {
        List<Long> agendaIds = meeting.getAgendas().stream().map(Agenda::getId).collect(Collectors.toList());
        return atmRepository.findbyCLientIdAndFileTypeAndObjIdInAndActiveTrue(BussinessCommon.getClientId(),
                FileTypeEnum.AGENDA, agendaIds);
    }

    public Calendar2 getByMeetingId(Long id) {
        Meeting meeting = meetingRepository.findByClientIdAndIdAndActiveTrue(BussinessCommon.getClientId(), id);
        Calendar2 calendar = new Calendar2(meeting);
        getOrgGroupParticipants(meeting, calendar);
        calendar.setParticipants(getUserNameList(meeting));
        calendar.setAttachments(getMeetingInviation(meeting));
        calendar.getAttachments().addAll(getMeetingAgendaAttachment(meeting));
        calendar.setParticipantsOrg(getListOrgCabinet(meeting));
        calendar.setParticipantsGroup(getListGroupCabinet(meeting));

        User user = userService.findByClientIdAndId(BussinessCommon.getClientId(), meeting.getCreateBy());
        Organization orgCreateBy = orgService.getOne(user.getOrg());

        if (orgCreateBy != null) {
            calendar.setOrgModel(orgCreateBy);
        }

        Organization parentOrg = orgService.getOne(orgService.findParentIdByOrgId(user.getOrg()));
        if (parentOrg != null) {
            calendar.setParentOrgName(parentOrg.getName());
        }

        List<DocumentCalendar> dInList = new ArrayList<>();
        List<DocumentCalendar> dOutList = new ArrayList<>();
        for (DocumentMeeting doc : meeting.getDInList()) {
            dInList.add(new DocumentCalendar(meeting.getId(), doc.getDocInId(), doc.getDocIn(), null, null,
                    DocumentTypeEnum.VAN_BAN_DEN));
        }
        for (DocumentMeeting doc : meeting.getDOutList()) {
            dOutList.add(new DocumentCalendar(meeting.getId(), null, null, doc.getDocOutId(), doc.getDocOut(),
                    DocumentTypeEnum.VAN_BAN_DI));
        }

        calendar.setDInList(dInList);
        calendar.setDOutList(dOutList);
        return calendar;
    }


    public CalendarWrapperDto getByWeek(Long orgType, Date start, Date end, int week, int year) {
        List<Calendar2> all = findByMonth(orgType, start, end);
        setAttachments(all);
        all.addAll(findCabinetByMonth(start, end));
        return new CalendarWrapperDto(setPage(all), start, end, week, year, attService.getAttCalWeek(week, year));
    }
    

    public List<CalendarStatusEnum> getStatus(int statusType) {
        if (statusType == Constant.CALENDAR_SCREEN_REGISTER) {
            return Arrays.asList(CalendarStatusEnum.PRE_APPROVE, CalendarStatusEnum.RETURN, CalendarStatusEnum.CANCEL);
        }
        return Arrays.asList(CalendarStatusEnum.APPROVE);
    }

    private boolean canEdit(User user, Calendar2 c, boolean isUserCanApprove) {
        if (orgService.isUserOfOrgType(user, Constant.BAN) && CalendarStatusEnum.APPROVE.equals(c.getStatus())
                && isUserCanApprove)
            return false;

        // CR: 14/1/2022 Cho phép chỉnh sửa lịch quá khứ
//		if (c.getStartTime() == null || (c.getStartTime().getTime() <= new Date().getTime()))
//			return false;

        return user.getId().equals(c.getCreateBy()) && !CalendarStatusEnum.APPROVE.equals(c.getStatus())
                || isUserCanApprove;
    }

    public boolean canCancel(Calendar2 c, boolean isUserCanApprove) {
        return CalendarStatusEnum.APPROVE.equals(c.getStatus()) && isUserCanApprove;
    }

    private boolean isStatusInArray(CalendarStatusEnum[] values, CalendarStatusEnum status) {
        return Arrays.stream(values).anyMatch(status::equals);
    }

    private boolean canDel(Calendar2 c, boolean isUserCanApprove) {
        User user = BussinessCommon.getUser();
        boolean isCreator = user.getId().equals(c.getCreateBy());
        CalendarStatusEnum[] values = {CalendarStatusEnum.CANCEL, CalendarStatusEnum.PRE_APPROVE, CalendarStatusEnum.RETURN};
        return isStatusInArray(values, c.getStatus()) && (isCreator || isUserCanApprove);
    }

	@Transactional
    public void addNotification(Calendar2 sCalendar) {
        //Delete thông báo
        notiService.deactiveAllByDocIdAndDocType(sCalendar.getId(), DocumentTypeEnum.TAO_LICH);
        //Add thông báo
        if (CalendarStatusEnum.PRE_APPROVE.equals(sCalendar.getStatus())) {
            addNotiRequestApproveCalendar(sCalendar);
        } else if (CalendarStatusEnum.APPROVE.equals(sCalendar.getStatus())) {
            NotificationHandleStatusEnum notiEnum = NotificationHandleStatusEnum.CAL_CAP_NHAT_CVV;
            if (Boolean.TRUE.equals(sCalendar.getRegisterBan()))
                notiEnum = NotificationHandleStatusEnum.CAL_CAP_NHAT_BAN;
            notiService.add(sCalendar.getCreateBy(), sCalendar.getId(), sCalendar.getTitle(), DocumentTypeEnum.TAO_LICH, notiEnum, sCalendar.isMeetingCalendar() ? ModuleCodeEnum.CAL_MEETING : ModuleCodeEnum.CAL_BUSINESS);
        }
    }

    public boolean del(Long calendarId) {
        User user = BussinessCommon.getUser();
        Calendar2 c = validCalendar(calendarId);
        boolean isUserCanApprove = c.isMeetingCalendar() ? checkUserCanApproveMeetingCalendar(user) : checkUserCanApprove(user, c);
        if (canDel(c, isUserCanApprove)) {
            c.setActive(false);
            calendar2Repository.save(c);
            return true;
        }

        throw new RestExceptionHandler(Message.NO_DEL_CALENDAR);
    }

    // statusType, //1-register /2-approve /3-publish
    // orgType, //1-Ban /2-CucVuVien /3-Phong
    public CalendarWrapperDto findByListCondition(Date start, Date end, int statusType, int orgType,
                                                  List<CalendarStatusEnum> status, int week, int year) {
        List<Calendar2> rsList = getList(start, end, statusType, orgType, status);
        setAttachments(rsList);
        return new CalendarWrapperDto(setPage(rsList), start, end, week, year, null);
    }

    private List<Calendar2> getList(Date start, Date end, int statusType, int orgType,
                                    List<CalendarStatusEnum> status) {
        User user = BussinessCommon.getUser();
        List<Long> orgIds;
        Long org = user.getOrg();
        List<Calendar2> rsList;
        if (!orgService.isUserOfOrgType(user, Constant.BAN)
                && !orgService.isUserOfOrgType(user, Constant.CUC_VU_VIEN)) {
            org = orgService.getParentByOrgType(user.getOrgModel(), Constant.CUC_VU_VIEN);
        }

        if (org == null) {
            org = orgService.getParentByOrgType(user.getOrgModel(), Constant.BAN);
        }

        orgIds = orgService.orgAndSub(org);

        if (statusType == Constant.CALENDAR_SCREEN_REGISTER) { // for register screen
            if (orgType == Constant.CALENDAR_ORG_TYPE_CUC_VU_VIEN) {
                if (orgService.isUserOfOrgType(user, Constant.BAN))
                    return Collections.emptyList();

                rsList = calendar2Repository.findByUnitLevel(user.getId(), false, start, end, orgIds, status, user.getClientId());

            } else {

                if (orgService.isUserOfOrgType(user, Constant.BAN)) {
                    rsList = calendar2Repository.findByTopLevel(true, false, start, end, orgIds, status,
                            user.getClientId());
                } else {
                    rsList = calendar2Repository.findByUnitLevel(user.getId(), true, start, end, orgIds, status, user.getClientId());
                }

            }
        } else { // for approve screen

            if (orgService.isUserOfOrgType(user, Constant.BAN)) {
                rsList = calendar2Repository.findByTopLevel(true, false, start, end, orgIds, status,
                        user.getClientId());
            } else {
                rsList = calendar2Repository.findByTopLevel(true, true, start, end, orgIds, status, user.getClientId());
            }
        }

        return rsList;
    }

    private List<Calendar2> setPage(List<Calendar2> page) {
        User user = BussinessCommon.getUser();
        if (BussinessCommon.isEmptyList(page)) {
            return Collections.emptyList();
        }

        for (Calendar2 i : page) {
            boolean isUserCanApprove = i.isMeetingCalendar() ? checkUserCanApproveMeetingCalendar(user) : checkUserCanApprove(user, i);
            i.setShowApproveBt(canApprove(i, false, isUserCanApprove));
            i.setShowRejectBt(canApprove(i, true, isUserCanApprove));
            i.setShowCancelBt(canCancel(i, isUserCanApprove));
            i.setParticipantsGuests(getListMemberByCalendarId(i.getId(), ReceiveTypeEnum.GUEST));
            i.setParticipantsPersonal(getListMemberByCalendarId(i.getId(), ReceiveTypeEnum.USER));
            i.setParticipantsOrg(getListOrgIngredient(i.getId()));
            if (i.getIsCabinet() != null && i.getIsCabinet() && BussinessCommon.getUserId().equals(i.getCreateBy())) {
                i.setShowDelBt(true);
                i.setShowEditBt(true);
            } else {
                i.setShowEditBt(canEdit(user, i, isUserCanApprove));
                i.setShowDelBt(canDel(i, isUserCanApprove));
            }
        }

        setCreateByUser(page);

        return page;
    }

    public boolean checkUserCanApproveMeetingCalendar(User user) {
        return authorityService.isUserHasAuthority(user.getId(), null, AuthorityEnum.APPROVE_MEETING_CALENDAR);
    }

    public boolean isRoomTimeDuplicate(Long roomId, Date start, Date end) {
        return calendar2Repository.isRoomTimeDuplicate(roomId, start, end, BussinessCommon.getClientId());
    }

    public CalendarWrapperDto findMeetingCalendar(Date start, Date end, int week, int year, Long roomId, Long userIdJoin) {
        User user = BussinessCommon.getUser();
        List<Calendar2> rsList;
        Long userId = user.getId();
        Long createBy = user.getId();

        if (checkUserCanApproveMeetingCalendar(user)) {
            userId = null;
            createBy = null;
        }

        rsList = calendar2Repository.findMeetingCalendar(userId, createBy, start, end, user.getClientId(), roomId, userIdJoin);
        setAttachments(rsList);
        return new CalendarWrapperDto(setPage(rsList), start, end, week, year, null);
    }

    public List<Calendar2> getToNow() {
        Date start = DateTimeUtils.getDateByWeek(0, 0, DateTimeUtils.TYPE_START_DATE);
        return calendar2Repository.getToNow(start, BussinessCommon.getUserId(), BussinessCommon.getClientId());
    }

    public List<Calendar2> getCalendar2toDate(String date, CalendarStatusEnum status, Long roomId) {
        Date dateSelect;
        try {
            dateSelect = DateTimeUtils.convertDate2TimestampSql(DateTimeUtils.convertDateFromStringPattern(date, "dd-MM-yyyy"), true, false);
        } catch (Exception e) {
            dateSelect = new Date();
        }
        return calendar2Repository.getCalendar2toDate(dateSelect, BussinessCommon.getClientId(), status, roomId);
    }

    public List<Calendar2> findByIds(List<Long> ids) {
        return calendar2Repository.findByClientIdAndActiveTrueAndIdIn(BussinessCommon.getClientId(), ids);
    }

    public boolean isMemberRelatedDoc(List<Long> userIds, Long docId, DocumentTypeEnum type) {
        return calendar2Repository.isMemberRelatedDoc(userIds, docId, BussinessCommon.getClientId(), type);
    }

    public List<Long> getListMemberByCalId(Long calId) {
        Calendar2 c = valid(calId, Message.NOT_FOUND_OBJECT);
        List<Long> rs = new ArrayList<>();
        rs.add(c.getCreateBy());
        String participants = c.getParticipants();
        if (StringUtils.isNullOrEmpty(participants)) {
            return rs;
        }
        String[] arr = participants.split(",");
        if (ArrayUtils.isEmpty(arr))
            return rs;
        rs.addAll(userService.findUserIdByFullName(arr));
        return rs;
    }

    public List<OrgGroupDto> getListOrgIngredient(Long calendarId) {
        return calendar2IngredientRepository.getListOrgIngredient(ReceiveTypeEnum.ORG, BussinessCommon.getClientId(), calendarId);
    }

    public List<Long> getListMemberInOrgByCalendarId(Long calendarId) {
        List<Long> rs = new ArrayList<>();
        List<OrgGroupDto> listOrg = calendar2IngredientRepository.getListOrgIngredient(ReceiveTypeEnum.ORG, BussinessCommon.getClientId(), calendarId);
        if (listOrg.size() > 0) {
            listOrg.forEach(i -> {
                rs.addAll(userService.findUserIdByOrgId(i.getId()));
            });
        }
        return rs;
    }

    public List<Long> getListMemberInGroupByCalendarId(Long calendarId) {
        List<Long> rs = new ArrayList<>();
        List<OrgGroupDto> listGroup = calendar2IngredientRepository.getListGroupIngredient(ReceiveTypeEnum.GROUP, BussinessCommon.getClientId(), calendarId);
        if (listGroup.size() > 0) {
            listGroup.forEach(i -> {
                rs.addAll(userRepository.getUserIdByGroupId(i.getId(), BussinessCommon.getClientId()));
            });
        }
        return rs;
    }

    public List<UserBasicDto> getListMemberByCalendarId(Long calendarId, ReceiveTypeEnum type) {
        return calendar2IngredientRepository.getListUserIngredient(type, BussinessCommon.getClientId(), calendarId);
    }

    public boolean isMemberCalendar(Long calId) {
        return calendar2Repository.isMemberCalendar(BussinessCommon.getUser(), calId, BussinessCommon.getClientId());
    }

    public void saveIngredient(Calendar2 calendar2, boolean isUpdate) {
        if (isUpdate) {
            List<Calendar2Ingredient> calendar2Ingredients = calendar2IngredientRepository.findOrgByCalendarId(BussinessCommon.getClientId(), calendar2.getId());
            calendar2IngredientRepository.deleteAll(calendar2Ingredients);
        }
        if (calendar2.getParticipantsGroup() != null) {
            for (OrgGroupDto orgGroupDto : calendar2.getParticipantsGroup()) {
                Calendar2Ingredient calendar2Ingredient = new Calendar2Ingredient();
                calendar2Ingredient.setObjectId(orgGroupDto.getId());
                calendar2Ingredient.setType(ReceiveTypeEnum.GROUP);
                calendar2Ingredient.setCalendarId(calendar2.getId());
                calendar2IngredientRepository.save(calendar2Ingredient);
            }
        }
        if (calendar2.getParticipantsOrg() != null) {
            for (OrgGroupDto orgGroupDto : calendar2.getParticipantsOrg()) {
                Calendar2Ingredient calendar2Ingredient = new Calendar2Ingredient();
                calendar2Ingredient.setObjectId(orgGroupDto.getId());
                calendar2Ingredient.setType(ReceiveTypeEnum.ORG);
                calendar2Ingredient.setCalendarId(calendar2.getId());
                calendar2IngredientRepository.save(calendar2Ingredient);
            }
        }
        if (calendar2.getParticipantsGuests() != null) {
            for (UserBasicDto userBasicDto : calendar2.getParticipantsGuests()) {
                Calendar2Ingredient calendar2Ingredient = new Calendar2Ingredient();
                calendar2Ingredient.setObjectId(userBasicDto.getId());
                calendar2Ingredient.setType(ReceiveTypeEnum.GUEST);
                calendar2Ingredient.setCalendarId(calendar2.getId());
                calendar2IngredientRepository.save(calendar2Ingredient);
            }
        }
        if (calendar2.getParticipantsPersonal() != null) {
            for (UserBasicDto userBasicDto : calendar2.getParticipantsPersonal()) {
                Calendar2Ingredient calendar2Ingredient = new Calendar2Ingredient();
                calendar2Ingredient.setObjectId(userBasicDto.getId());
                calendar2Ingredient.setType(ReceiveTypeEnum.USER);
                calendar2Ingredient.setCalendarId(calendar2.getId());
                calendar2IngredientRepository.save(calendar2Ingredient);
            }
        }
    }

    private List<Participant> updateParticipantCalendar(Long meetingId, ReceiveTypeEnum type, List<Long> newIds,
                                                        List<Participant> oldParticipants, Long groupId) {
        List<Participant> rsParticipant = new ArrayList<>();

        Map<Long, Participant> pMap = new HashMap<>();
        for (Participant p : oldParticipants) {
            pMap.put(p.getObjId(), p);
        }

        for (Long p : newIds) {
            if (pMap.containsKey(p)) {
                rsParticipant.add(pMap.get(p));
            } else {
                if (ReceiveTypeEnum.ORG.equals(type)) {
                    rsParticipant.add(new Participant(p, ReceiveTypeEnum.ORG, RoleEnum.MEMBER, meetingId, null));
                } else if (ReceiveTypeEnum.USER.equals(type)) {
                    rsParticipant.add(new Participant(p, ReceiveTypeEnum.USER, RoleEnum.MEMBER, meetingId, null));
                } else {
                    rsParticipant.add(new Participant(p, ReceiveTypeEnum.GROUP, RoleEnum.MEMBER, meetingId, groupId));
                }
            }
        }

        return rsParticipant;
    }

    private Meeting saveDocRelated(Meeting m, List<DocumentMeeting> dInList, List<DocumentMeeting> dOutList) {
        List<DocumentMeeting> nInList = new ArrayList<>();
        List<DocumentMeeting> nOutList = new ArrayList<>();
        if (dInList != null)
            dInList.forEach(i -> nInList.add(new DocumentMeeting(m, i.getDocInId(), DocumentTypeEnum.VAN_BAN_DEN)));
        if (dOutList != null)
            dOutList.forEach(i -> nOutList.add(new DocumentMeeting(m, i.getDocOutId(), DocumentTypeEnum.VAN_BAN_DI)));
        m.getDInList().clear();
        m.getDOutList().clear();

        m.setDInList(nInList, m.getId());
        m.setDOutList(nOutList, m.getId());

        try {
            return meetingRepository.save(m);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RestExceptionHandler(Message.INVALID_INPUT_DATA);
        }
    }

    private void deleteParticipantAfterUpdating(List<Participant> newP, List<Participant> oldP) {
        List<Long> newIds = newP.stream().map(Participant::getId).collect(Collectors.toList());
        List<Long> delete = new ArrayList<>();
        for (Participant p : oldP) {
            if (!newIds.contains(p.getId())) {
                delete.add(p.getId());
            }
        }
        participantRepository.deleteById(delete, BussinessCommon.getClientId());
    }

	@Transactional
    public Meeting updateMeetingCalendar(Long meetingId, MeetingCalendarDto data) {
        // valids meeting and creator
        Meeting meeting = meetingRepository.findByClientIdAndIdAndActiveTrue(BussinessCommon.getClientId(), meetingId);
        if (meeting == null)
            throw new RestExceptionHandler(Message.MEETING_CALENDAR_NOT_FOUND);
        if (!BussinessCommon.getUserId().equals(meeting.getCreateBy()))
            throw new RestExceptionHandler(Message.NO_ACTION_PERMISSION);

        if (!data.getDescription().equals(meeting.getSchedule())) {
            AttachmentMeeting schedule = atmRepository.findByClientIdAndObjIdAndFileTypeAndActiveTrue(BussinessCommon.getClientId(), meetingId, FileTypeEnum.SCHEDULE);
            if (schedule != null) {
                storageService.deleteFile(schedule.getName());
                atmRepository.delete(schedule);
            }
            meeting.setScdType(Constant.TEXT);
            meeting.setSchedule(data.getDescription());
        }
        meeting.setCalendarToMeeting(data);
        //update docs and save meeting
        saveDocRelated(meeting, data.getDInList(), data.getDOutList());


        // update participants
        List<Participant> rsParticipant = new ArrayList<>();
        if (!StringUtils.isNullOrEmpty(data.getParticipants())) {
            List<Long> tmpUser = userService.getIdsByFullNames(data.getParticipants().split(","));
            rsParticipant.addAll(updateParticipantCalendar(meetingId, ReceiveTypeEnum.USER, tmpUser,
                    meeting.getParticipants(), null));
        }
        if (data.getParticipantsOrg() != null) {
            List<Long> tmpOrg = data.getParticipantsOrg().stream().map(OrgGroupDto::getId).collect(Collectors.toList());
            rsParticipant.addAll(
                    updateParticipantCalendar(meetingId, ReceiveTypeEnum.ORG, tmpOrg, meeting.getParticipants(), null));
        }
        if (data.getParticipantsGroup() != null) {
            List<Long> tmpGroup = data.getParticipantsGroup().stream().map(OrgGroupDto::getId)
                    .collect(Collectors.toList());
            for (Long groupId : tmpGroup) {
                List<Long> participantsGroup = new ArrayList<>();
                List<Long> userIds = groupUserRepository
                        .findByClientIdAndGroupIdAndActiveTrue(BussinessCommon.getClientId(), groupId).stream()
                        .map(GroupUser::getUserId).collect(Collectors.toList());
                participantsGroup.addAll(userIds);
                rsParticipant.addAll(updateParticipantCalendar(meetingId, ReceiveTypeEnum.GROUP, participantsGroup,
                        meeting.getParticipants(), groupId));
            }
        }
        participantRepository.saveAll(rsParticipant);
        deleteParticipantAfterUpdating(rsParticipant, meeting.getParticipants());

        return meeting;
    }

    public List<AttachmentMeeting> addListAttachment(MultipartFile[] files, Long meetingId) {

        if (BussinessCommon.isEmptyArr(files)) {
            return Collections.emptyList();
        }

        Meeting meeting = meetingRepository.findByClientIdAndIdAndActiveTrue(BussinessCommon.getClientId(), meetingId);
        Long firstAgendaId = meeting.getAgendas().get(0).getId();
        List<AttachmentMeeting> aList = new ArrayList<>();
        for (MultipartFile f : files) {
            AttachmentMeeting a = new AttachmentMeeting();
            a.setName(storageService.save(f));
            a.setType(f.getContentType());
            a.setSize(f.getSize());
            a.setObjId(firstAgendaId);
            a.setFileType(FileTypeEnum.AGENDA);
            aList.add(a);
        }
        return atmRepository.saveAll(aList);
    }

    public HashMap<String, Boolean> deleteMeetingCalendarAttachmentById(Long id, Long meetingId) {

        //check if deleted file exist in database
        AttachmentMeeting attachment = atmRepository.findByClientIdAndIdAndActiveTrue(BussinessCommon.getClientId(), id);
        if (attachment == null) {
            throw new RestExceptionHandler(Message.MEETING_CALENDAR_ATTACHMENT_NOT_FOUND);
        }

        //check if deleted file is the invitation
        Meeting meeting = meetingRepository.findByClientIdAndIdAndActiveTrue(BussinessCommon.getClientId(), meetingId);
        if (meeting.getInv().equals(attachment.getName())) {
            throw new RestExceptionHandler(Message.MEETING_CALENDAR_INVITATION_DELETE_WARNING);
        }

        //delete file in storage
        if (Boolean.TRUE.equals(attachment.getEncrypt())) {
            encryptService.delEncrypt(attachment.getName());
        } else {
            storageService.deleteFile(attachment.getName());
        }

        //delete file in databse
        atmRepository.delete(attachment);
        HashMap<String, Boolean> stringBooleanHashMap = new HashMap<>();
        stringBooleanHashMap.put("deleted", Boolean.TRUE);
        return stringBooleanHashMap;
    }

    public Boolean deleteMeetingCalendarById(Long id) {
        Meeting meeting = meetingRepository.findByClientIdAndIdAndActiveTrue(BussinessCommon.getClientId(), id);
        if (meeting == null)
            throw new RestExceptionHandler(Message.MEETING_CALENDAR_NOT_FOUND);

        meeting.setActive(false);
        meetingRepository.save(meeting);
        return true;
    }

    public void checkFullNameNotFound(String participants) {
        if (participants != null) {
            String[] arr = participants.split(",");
            for (String fullName : arr) {
                if (!fullName.trim().equals("")) {
                    int index = fullName.indexOf("-");
                    String name = index > 0 ? fullName.substring(0, index).toLowerCase().trim() : fullName.toLowerCase().trim();
                    if (!userRepository.getUserByFullName(name, BussinessCommon.getClientId())) {
                        throw new RestExceptionHandler("Cá nhân: " + fullName + " không tồn tại trong hệ thống. Mời	 bạn nhập tên này vào trường thành phần !");
                    }
                }

            }
        }
    }

    public Boolean createCalendarBan() {
        return iAuthorityUserRepository.checkAuthorByPermission(AuthorityEnum.APPROVE_TOP_LEVEL_CALENDAR, BussinessCommon.getClientId(), BussinessCommon.getUserId());
    }

    public List<AttachmentMeeting> saveFileMeeting(MultipartFile[] files, FileTypeEnum type, Long objId) {
        List<AttachmentMeeting> rs = new ArrayList<>();
        if (ArrayUtils.isEmpty(files))
            return rs;

        String name;
        AttachmentMeeting tmp;
        for (MultipartFile file : files) {
            name = storageService.save(file, root);
            tmp = new AttachmentMeeting(file, objId, name, type);
            rs.add(atmRepository.save(tmp));
        }

        return rs;
    }

    public Boolean delFileMeeting(String[] names, FileTypeEnum type) {
        if (BussinessCommon.isEmptyArr(names)) {
            return false;
        }

        AttachmentMeeting tmp;
        boolean rs = true;
        for (String i : names) {
            tmp = findByNameAndFileType(i, type, false);
            if (tmp == null) {
                rs = false;
            }
            delete(tmp);
        }
        return rs;
    }

    public AttachmentMeeting findByNameAndFileType(String name, FileTypeEnum type, boolean checkErr) {
        AttachmentMeeting atm = atmRepository.findByNameAndFileTypeAndClientIdAndActiveTrue(name, type,
                BussinessCommon.getClientId());
        if (checkErr && (atm == null || Boolean.FALSE.equals(atm.getActive()))) {
            throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
        }
        return atm;
    }

    public void delete(AttachmentMeeting a) {
        if (a == null)
            return;
        storageService.delete(a.getName());
        a.setActive(false);
        atmRepository.save(a);
    }

    public Calendar2 updateCalendar(Long id, CalendarStatusEnum status, String comment) {
        User user = BussinessCommon.getUser();

        BussinessCommon.validLengthData(comment, "Ý kiến xử lý", 200);

        if (!roleService.isAllowModule(user, ModuleCodeEnum.CAL_BUSINESS.getName(), ModuleCodeEnum.CAL_MEETING.getName()))
            throw new RestExceptionHandler(Message.CALENDER_NOT_ALLOW);

        Calendar2 c = getCalendar2(id);
        boolean canApporve = c.isMeetingCalendar() ? checkUserCanApproveMeetingCalendar(user) : canApprove(user, c);
        if (!canApporve) throw new RestExceptionHandler(Message.CALENDER_NOT_ALLOW);

        c.setStatus(status);
        c.setComment(comment);
        Calendar2 sCalendar = calendar2Repository.save(c);

        //Xóa tất cả thông báo
        notiService.deactiveAllByDocIdAndDocType(sCalendar.getId(), DocumentTypeEnum.TAO_LICH);
        //Add thông báo cho người tạo.
        NotificationHandleStatusEnum notiEnum = NotificationHandleStatusEnum.CAL_DONG_Y_CVV;
        if (CalendarStatusEnum.APPROVE.equals(status)) {
            notiEnum = NotificationHandleStatusEnum.CAL_DONG_Y_CVV;
            calendarCommentService.saveCmt(new CalendarComment(id, "Duyệt lịch họp: " + comment, user.getId()));
            if (Boolean.TRUE.equals(sCalendar.getRegisterBan())) notiEnum = NotificationHandleStatusEnum.CAL_DONG_Y_BAN;
        } else if (CalendarStatusEnum.RETURN.equals(status)) {
            notiEnum = NotificationHandleStatusEnum.CAL_TU_CHOI_CVV;
            calendarCommentService.saveCmt(new CalendarComment(id, "Từ chối lịch họp: " + comment, user.getId()));
            if (Boolean.TRUE.equals(sCalendar.getRegisterBan()))
                notiEnum = NotificationHandleStatusEnum.CAL_TU_CHOI_BAN;
        } else if (CalendarStatusEnum.CANCEL.equals(status)) {
            notiEnum = NotificationHandleStatusEnum.CAL_HUY_DUYET_CVV;
            if (Boolean.TRUE.equals(sCalendar.getRegisterBan()))
                notiEnum = NotificationHandleStatusEnum.CAL_HUY_DUYET_BAN;
        }
        notiService.add(sCalendar.getCreateBy(), sCalendar.getId(), sCalendar.getTitle(), DocumentTypeEnum.TAO_LICH, notiEnum, sCalendar.isMeetingCalendar() ? ModuleCodeEnum.CAL_MEETING : ModuleCodeEnum.CAL_BUSINESS);

        calendarHistoryservice.save(null, sCalendar, CalendarActionEnum.APPROVE);

        //Cập nhật cuộc họp bên Office
        updateMeetingEcabinet(sCalendar);
        return sCalendar;
    }

    private void updateMeetingEcabinet(Calendar2 sCalendar) {
        Meeting meetingEcabinet = calendar2Repository.getMeetingByCalendarId(BussinessCommon.getClientId(), sCalendar.getId());
        if (meetingEcabinet != null) {
            if (meetingEcabinet.getStatus().equals(MeetingStatusEnum.DANG_HOP)) {
                throw new RestExceptionHandler("Cuộc họp đang diễn ra nên không cập nhật ECABINET");
            } else {
                meetingEcabinet.setSubject(sCalendar.getTitle());
                meetingEcabinet.setTypeId(sCalendar.getCatTypeId());
                meetingEcabinet.setStart(sCalendar.getStartTime());
                meetingEcabinet.setEnd(sCalendar.getEndTime());
                meetingEcabinet.setPlaceId(sCalendar.getRoomId());
                meetingEcabinet.setSchedule(sCalendar.getDescription());
                meetingEcabinet.setHostOrgId(sCalendar.getOrgMeeting());
                meetingEcabinet.setHostUserId(sCalendar.getChairmanId());
                meetingEcabinet.setNote(sCalendar.getNote());

                //Cập nhật thành phần tham dự
                List<Participant> participantsOld = participantRepository.findByClientIdAndMeetingIdAndActiveTrue(BussinessCommon.getClientId(), meetingEcabinet.getId());
                participantRepository.deleteAll(participantsOld);
                List<Participant> participantPersonals = new ArrayList<>();
                sCalendar.getParticipantsPersonal().forEach(i -> participantPersonals.add(new Participant(i.getId(), ReceiveTypeEnum.USER, RoleEnum.MEMBER, meetingEcabinet.getId(), null)));
                List<Participant> participantGuests = new ArrayList<>();
                sCalendar.getParticipantsGuests().forEach(i -> participantGuests.add(new Participant(i.getId(), ReceiveTypeEnum.USER, RoleEnum.GUEST, meetingEcabinet.getId(), null)));
                List<Participant> participantOrgs = new ArrayList<>();
                sCalendar.getParticipantsPersonal().forEach(i -> participantOrgs.add(new Participant(i.getId(), ReceiveTypeEnum.ORG, RoleEnum.MEMBER, meetingEcabinet.getId(), null)));
                participantRepository.saveAll(participantPersonals);
                participantRepository.saveAll(participantGuests);
                participantRepository.saveAll(participantOrgs);

                // Cập nhật văn bản đính kèm
                saveDocMeetingRelated(meetingEcabinet, sCalendar.getDInList(), sCalendar.getDOutList());
            }
        }
    }

    public Calendar2 updateBodyCalendar(Long id, Calendar2 news) {
        User user = BussinessCommon.getUser();
        boolean isChangeStatus = true;

        Calendar2 old = findByIdAndClientIdAndActive(id, true);
        if (old == null || news == null
            // CR: 14/1/2022 Cho phép chỉnh sửa lịch quá khứ
//				|| old.getStartTime().getTime() <= new Date().getTime()
        ) {
            throw new RestExceptionHandler(Message.CALENDAR_INVALD);
        }

        if (!roleService.isAllowModule(user, ModuleCodeEnum.CAL_BUSINESS.getName(), ModuleCodeEnum.CAL_MEETING.getName()))
            throw new RestExceptionHandler(Message.CALENDER_NOT_ALLOW);

        boolean canApprove = old.isMeetingCalendar() ? checkUserCanApproveMeetingCalendar(user)
                : checkUserCanApprove(user, old);

        if (!canApprove && !user.getId().equals(old.getCreateBy())) {
            throw new RestExceptionHandler(Message.CALENDER_NOT_ALLOW);
        }

        news.validCalender();
        List<DocumentCalendar> docInList = news.getDInList();
        List<DocumentCalendar> docOutList = news.getDOutList();
        List<DocumentCalendar> taskList = news.getTaskList();
        checkFullNameNotFound(news.getParticipants());
        saveIngredient(news, true);
        if (old.isMeetingCalendar()) {
            old.setCalendater(false, news);
            Calendar2 sCalendar1 = saveDocRelated(old, docInList, docOutList, taskList);
            //calendar2Service.save(old);

            addNotification(sCalendar1);
            calendarHistoryservice.save(null, sCalendar1, CalendarActionEnum.UPDATE);

            //Cập nhật bên Ecabinet
            updateMeetingEcabinet(sCalendar1);

            return sCalendar1;
        }

        if (CalendarStatusEnum.APPROVE.equals(old.getStatus())) {
            if (canApprove) isChangeStatus = false;
            if (Boolean.TRUE.equals(news.getRegisterBan())) {
                news.setId(null);
                news.setCalendater(true, news);
                Calendar2 sCalendar = save(news);
                saveDocRelated(sCalendar, docInList, docOutList, taskList);
                // Add thông báo
                addNotiRequestApproveCalendar(sCalendar);
                calendarHistoryservice.save(null, sCalendar, CalendarActionEnum.REGISTER_TOP_LEVEL);

                //Cập nhật bên Ecabinet
                updateMeetingEcabinet(sCalendar);
                return sCalendar;
            }
        }

        old.setCalendater(isChangeStatus, news);
        if (Boolean.TRUE.equals(old.getRegisterBan())) {
            old.setRegisterBan(false);
            Calendar2 calendarForBan = new Calendar2();
            calendarForBan.setCalendater(false, old);
            calendarForBan.setMeetingCalendar(false);
            calendarForBan.setOrgId(orgService.getRootOrgId(user.getOrg()));
            calendarForBan = calendar2Repository.save(calendarForBan);
            addNotiRequestApproveCalendar(calendarForBan);
        }
        Calendar2 sCalendar = saveDocRelated(old, docInList, docOutList, taskList);
//		Calendar2 sCalendar = calendar2Service.save(old);

        addNotification(sCalendar);
        calendarHistoryservice.save(null, sCalendar, CalendarActionEnum.UPDATE);

        //Cập nhật bên Ecabinet
        updateMeetingEcabinet(sCalendar);
        return sCalendar;
    }

    public List<CalendarDto> getMeetingPerson(Date start, Date end, int week, int year) {
        List<Calendar2> rsList = calendar2Repository.findMeetingPerson(BussinessCommon.getUserId(), start, end, BussinessCommon.getClientId());
        Map<Integer, List<Calendar2>> mapByDate = new CalendarWrapperDto().setCalendarListByDate(rsList);
        List<CalendarDto> res = new ArrayList<>();
        mapByDate.forEach((key, value) -> {
            List<Calendar2DetailDto> listByDate = new ArrayList<>();
            value.forEach(i -> {
                Calendar2DetailDto dto = new Calendar2DetailDto();
                dto.set(i);
                listByDate.add(dto);
            });
            Date date = DateTimeUtils.getDateByDateThAndWeek(key, week, year);
            Boolean isToday = DateTimeUtils.getDateNotTime(date).compareTo(start) == 0;
            res.add(new CalendarDto(DateTimeUtils.dayOfWeekMap.get(key), date, listByDate, isToday));

        });
        return res;
    }
}
