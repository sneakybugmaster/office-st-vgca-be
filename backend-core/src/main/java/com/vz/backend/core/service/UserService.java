package com.vz.backend.core.service;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.*;
import com.vz.backend.core.domain.Module;
import com.vz.backend.core.domain.*;
import com.vz.backend.core.dto.*;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.*;
import com.vz.backend.util.DateTimeUtils;
import com.vz.backend.util.PasswordUtils;
import com.vz.backend.util.StringUtils;
import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

@Service
public class UserService extends BaseService<User> {

    @Value("${configs.user.find_by_org_and_sub: true}")
    private boolean findByOrgAndSub;
    @Value("${configs.clerical-org: false}")
    private boolean clericalOrg;
    @Value("${configs.ldap2.orgId:}")
    private Long ldapOrgId;

    @Value("${configs.ldap2.positionId:}")
    private Long ldapPositionId;

    @Value("${configs.ldap2.clientId:1}")
    private Long ldapClientId;

    @Value("${configs.ldap2.updateBy:}")
    private Long ldapUpdateBy;

    @Value("${configs.ldap2.mail:@vgisc.com}")
    private String ldapMail;

    @Autowired
    private IUserRepository userRepository;

    @Autowired
    CategoryTypeService catTypeService;

    @Autowired
    CategoryService catService;

    @Autowired
    OrganizationService orgService;

    @Autowired
    DelegateFlowService delegateFlowService;

    @Autowired
    RoleService rService;

    @Autowired
    AuthorityUserService authorityService;

    @Autowired
    SecretaryService secretaryService;

    @Autowired
    MailNotiService mailNotiService;

    @Autowired
    private IRoleRepository roleRepository;

    @Autowired
    private IModuleRepository moduleRepository;

    @Autowired
    private IPermissionRepository permissionRepository;

    @Autowired
    private IUserRoleRepository userRoleRepository;

    @Autowired
    private ICategoryRepository categoryRepository;

    @Autowired
    private IUserCategoryRepository userCategoryRepository;

    @Autowired
    private IUserOrganizationRepository userOrganizationRepository;


    @Override
    public IRepository<User> getRepository() {
        return userRepository;
    }

    public User findByUserNameAndActive(String userName, boolean active) {
        return userRepository.findByUserNameAndActive(userName, active);
    }

    public User findBySerialToken(String token) {
        return userRepository.findBySerialTokenAndActiveTrue(token);
    }

    public User findByUserNameAndPasswordAndActive(String userName, String password, boolean active) {
        return userRepository.findByUserNameAndPasswordAndActive(userName, password, active);
    }

    public User findByUserNameAndClientId(String userName, Long clientId) {
        return userRepository.findByUserNameAndClientId(userName, clientId);
    }

    public List<User> findByListUserNameAndClientId(Long[] userName, Long clientId) {
        return userRepository.findByListUserNameAndClientId(userName, clientId);
    }

    public List<User> findByOrgIdAndClientId(Long orgId, Long clientId) {
        return userRepository.findByOrgAndClientId(orgId, clientId);
    }

    public List<User> findListUserByOrgAndPositionId(Long orgId, Long posId) {
        return userRepository.findListUserByOrgAndPositionIdAndActive(orgId, posId, true, BussinessCommon.getClientId());
    }

    public Page<User> findUser(String fullName, String userName, String email, String phone, Long sex, String indentity,
                               String title, String nameToken, String serialToken, Long employeeId, String employeeCode, String salt,
                               Long org, Long position, Boolean lead, Date birthday, Pageable pageable) {
        List<Long> orgIds = null;
        if (findByOrgAndSub) {
            orgIds = org == null ? null : orgService.orgAndSub(org);
        } else {
            orgIds = org == null ? null : Arrays.asList(org);
        }

        Date frBirthday = DateTimeUtils.handleSubmit(birthday, Calendar.MILLISECOND, -1);
        Date toBirthday = DateTimeUtils.handleSubmit(birthday, Calendar.DAY_OF_MONTH, 1);
        Page<User> userPage = userRepository.findUser(fullName, userName, email, phone, sex, indentity, title, nameToken, serialToken,
                employeeId, employeeCode, salt, orgIds, position, lead, frBirthday, toBirthday, BussinessCommon.getClientId(), pageable);
        for (User user : userPage.getContent()) {
            if (user.getOrgModel().getParentId() != null) {
                Organization organization = orgService.findByClientIdAndId(user.getOrgModel().getParentId());
                user.setOrgParent(organization != null ? organization.getName() : "");
            } else {
                user.setOrgParent("");
            }
            setAllOrgAndPositionUser(user);
        }
        return userPage;
    }

    public User setAllOrgAndPositionUser(User user) {
        List<Organization> userOrgs = userOrganizationRepository.getAllOrgByUserId(user.getClientId(), user.getId(), true);
        List<CategoryDto> userCategories = userCategoryRepository.findCategoryByUserIdAndActive(user.getId(), user.getClientId());
        user.setAdditionalOrganizations(userOrgs);
        user.setAdditionalPositions(userCategories);
        return user;
    }

    public List<User> findListUserByPosition(Long positionId) {
        return userRepository.findByClientIdAndPosition(BussinessCommon.getClientId(), positionId);
    }

    public List<UserInfoDto> getByClientIdAndActiveAndSort(String direction, Boolean active, String column) {
        Sort sort;
        if (direction.equals("ASC")) {
            sort = Sort.by(Direction.ASC, column);
        } else {
            sort = Sort.by(Direction.DESC, column);
        }
        List<User> data = userRepository.findByClientIdAndActive(BussinessCommon.getClientId(), active, sort);
        List<Organization> oList = orgService.findByClientId(BussinessCommon.getClientId());
        CategoryType type = catTypeService.findByClientIdAndCode(BussinessCommon.getClientId(), Constant.CAT_POSITION);
        if (type == null) {
            throw new RestExceptionHandler(Message.NOT_FOUND_POSITION);
        }
        List<Category> cList = catService.findByClientIdAndCategoryTypeId(BussinessCommon.getClientId(), type.getId(),
                null);
        List<UserInfoDto> dtoList = new ArrayList<>();
        data.stream().forEach(u -> {
            UserInfoDto dto = new UserInfoDto();
            dto.setId(u.getId());
            dto.setFullName(u.getFullName());
            dto.setOrgId(u.getOrg());
            dto.setPositionId(u.getPosition());
            dto.setUserName(u.getUserName());
            oList.forEach(o -> {
                if (o.getId().equals(u.getOrg())) {
                    dto.setOrgName(o.getName());
                }
            });
            cList.forEach(c -> {
                if (c.getId().equals(u.getPosition())) {
                    dto.setPositionName(c.getName());
                }
            });
            dto.setLead(u.isLead());
            dtoList.add(dto);
        });
        return dtoList;
    }

    public Page<User> getAllUser(Integer page) {
        return userRepository.getAllUser(BussinessCommon.getClientId(), BussinessCommon.castToPageable(page));
    }

    public List<UserInfoDto> getAllUserByLead() {
        CategoryType type = catTypeService.findByClientIdAndCode(BussinessCommon.getClientId(), Constant.CAT_POSITION);
        if (type == null) {
            throw new RestExceptionHandler(Message.NOT_FOUND_POSITION);
        }
        List<Category> cList = catService.findByClientIdAndCategoryTypeId(BussinessCommon.getClientId(), type.getId(),
                null);
        List<UserInfoDto> dtoList = userRepository.getAllUserByLead(BussinessCommon.getClientId(), true);
        List<Organization> oList = orgService.findByClientId(BussinessCommon.getClientId());
        dtoList.forEach(dto -> {
            cList.forEach(c -> {
                if (c.getId().equals(dto.getPositionId())) {
                    dto.setPositionName(c.getName());
                }
            });
            oList.forEach(o -> {
                if (o.getId().equals(dto.getOrgId())) {
                    dto.setOrgName(o.getName());
                }
            });
        });
        return dtoList;
    }

    public List<User> searchUserActive(String textSearch) {
        return userRepository.searchUserActive(StringUtils.handleSubmit(textSearch), BussinessCommon.getClientId());
    }

    public List<SignerDto> searchUserDtoActive(String textSearch) {
        return userRepository.searchUserSign(StringUtils.handleSubmit(textSearch), BussinessCommon.getClientId());
    }

    public String getFullNameById(Long userId) {
        return userRepository.getFullNameById(userId, BussinessCommon.getClientId());
    }

    public List<UserDto> getAllUserDtoByActive() {
        return userRepository.getAllUserDtoByActive(BussinessCommon.getClientId());
    }

    public List<User> findByIds(Long[] listIds) {
        return userRepository.findByIds(listIds);
    }

    public List<User> findByIds(List<Long> listIds, boolean active) {
        return userRepository.findByIds(listIds, BussinessCommon.getClientId(), active);
    }

    public List<Long> getIdList(Boolean active) {
        return userRepository.getIdList(BussinessCommon.getClientId(), active);
    }

    public List<Long> getListLeadUserIdByOrg(List<Long> org) {
        return userRepository.getListLeadUserIdByOrg(org);
    }

    public List<User> getListLeadUserIdByOrg(Long org) {
        Long[] orgArr = {org};
        return getListLeadByOrg(orgArr);
    }

    public List<User> getListLeadByOrg(Long[] org) {
        return userRepository.getListLeadByOrg(org, BussinessCommon.getClientId());
    }

    public List<Long> getLeadBasicByOrgs(Long[] orgs) {
        return userRepository.getLeadBasicByOrgs(orgs, BussinessCommon.getClientId());
    }

    public List<String> findUserNameByKeys(String key) {
        return userRepository.findUserNameByKeys(key, BussinessCommon.getClientId());
    }

    public List<Long> findIdByKeys(String key) {
        return userRepository.findIdByKeys(key, BussinessCommon.getClientId());
    }

    /**
     * @param userName
     * @return
     */
    public User findByUserName(String userName) {
        User user = userRepository.findByUserNameAndClientId(userName, BussinessCommon.getClientId());
        if (user != null) {
            user.setAuthoritys(authorityService.get(user.getId(), BussinessCommon.getClientId(), true));
            user.setCecretarys(secretaryService.findSecretaryByBossId(user.getId(), true));
            List<Organization> listOrg = userOrganizationRepository.getAllOrgByUserId(BussinessCommon.getClientId(), user.getId(), true);
            for (Organization org : listOrg) {
                List<Category> positionInOrg = findCategoryByUserIdAndActive(user.getId(), true, BussinessCommon.getClientId(), org.getId());
                org.setPositions(positionInOrg);
            }
            user.setAdditionalOrganizations(listOrg);
        }
        return user;
    }

    public User findByUserId(Long userId) {
        return userRepository.findByIdAndClientId(userId, BussinessCommon.getClientId());
    }

    public User findByUserNameForLdap(String userName, Long clientId) {
        return userRepository.findByUserNameAndClientId(userName, clientId);
    }

    public List<UserTreeDto> getAllOrder() {
        List<UserTreeDto> treeUsers = userRepository.getAllOrder1(ModuleCodeEnum.DOCUMENT_IN.getName(), BussinessCommon.getClientId());
        Set<Long> ids = treeUsers.stream().map(UserTreeDto::getId).collect(Collectors.toSet());
        Set<Long> directAuthors = authorityService.checkAuthorByIds(ids, AuthorityEnum.DIRECTION_DOCUMENT);
        treeUsers.forEach(i -> i.setDirectionAuthority(directAuthors.contains(i.getId())));
        return treeUsers;
    }

    public List<User> findByPositionInAndActiveAndClientIdOrderByFullName(List<Long> listPosId, boolean active,
                                                                          Long clientId) {
        return userRepository.findByPositionInAndActiveAndClientIdOrderByFullName(listPosId, active, clientId);
    }

    public List<Role> findRoleByUserIdAndActive(User user, boolean active, long clientId, Boolean cabinet) {
        return userRepository.findRoleByUserIdAndActive(user.getId(), user.getPosition(), active, clientId, cabinet);
    }

    public List<Category> findCategoryByUserIdAndActive(long userId, boolean active, long clientId, Long orgId) {
        return userCategoryRepository.findCategoryByUserIdAndOrgIdAndActive(userId, active, clientId, orgId);
    }

    public List<Long> findRoleIdByUserIdAndActive(User user, boolean active, long clientId) {
        return userRepository.findRoleIdByUserIdAndActive(user.getId(), user.getPosition(), active, clientId);
    }

    @Transactional
    public void setCurrentRole(long userId, long roleId) {
        userRepository.setCurrentRole(userId, roleId, BussinessCommon.getClientId());
    }

    public User validUserId(Long userId) {
        User user = findByClientIdAndId(BussinessCommon.getClientId(), userId);
        if (user == null) {
            throw new RestExceptionHandler(Message.INVALID_ACCOUNT);
        }
        return user;
    }

    public User validUserId(Long userId, Long clientId) {
        User user = findByClientIdAndId(clientId, userId);
        if (user == null) {
            throw new RestExceptionHandler(Message.INVALID_ACCOUNT);
        }
        return user;
    }

    public boolean existUserByRoleIdAndActiveAndClientId(long userId, long roleId, boolean active, long clientId) {
        return userRepository.existUserByRoleIdAndActiveAndClientId(userId, roleId, active, clientId);
    }

    public List<User> getListUserByModuleCodeAndClientId(List<String> listCode, long clientId) {
        return userRepository.getListUserByModuleCodeAndActiveAndClientId(listCode, true, clientId);
    }

    public List<Long> getListUserIdByModuleCodeAndClientId(List<String> listCode, long clientId) {
        return userRepository.getListUserIdByModuleCodeAndActiveAndClientId(listCode, true, clientId);
    }

    public boolean checkUserIdByModuleCodeAndClientId(long userId, List<String> listCode, long clientId) {
        return userRepository.checkUserIdByModuleCodeAndActiveAndClientId(userId, listCode, true, clientId);
    }

    public List<User> getListUserByModuleCodeAndOrgAndClientId(List<String> code, long orgId, long clientId) {
        return userRepository.getListUserByModuleCodeAndOrgAndClientIdAndActive(code, orgId, clientId, true);
    }

    public List<Long> getListUserIdByModuleCodeAndOrgAndClientId(List<String> code, long orgId, long clientId) {
        return userRepository.getListUserIdByModuleCodeAndOrgAndClientIdAndActive(code, orgId, clientId, true);
    }

    public List<String> getListUserNameByModuleCodeAndOrgAndClientId(List<String> code, long orgId, long clientId) {
        return userRepository.getListUserNameByModuleCodeAndOrgAndClientIdAndActive(code, orgId, clientId, true);
    }

    public boolean checkUserIdByModuleCodeAndOrgAndClientId(long userId, long orgId, List<String> listCode, long clientId) {
        return userRepository.checkUserIdByModuleCodeAndOrgAndActiveAndClientId(userId, orgId, listCode, true, clientId);
    }

    public long getOrgIdByUserName(String userName) {
        return userRepository.findOrgIdByUserNameAndActiveAndClientId(userName, true, BussinessCommon.getClientId());
    }

    public List<User> allUserInOrg(Long orgId, String textSearch, Boolean active) {
        List<String> listPb = Arrays.asList("Phó Giám Đốc");
        List<String> listTb = Arrays.asList("Giám Đốc");
        Organization organization = orgService.getOrgCucByOrg(BussinessCommon.getUser().getOrgModel());
        List<Long> orgIds = orgId != null ? Collections.singletonList(orgId) : orgService.orgAndSub(organization.getId());
        List<User> usersData = userRepository.findUserByOrgInAndClientIdAndActive(orgIds, textSearch, BussinessCommon.getClientId(), active);
        List<User> listUserLDC = new ArrayList<>();
        List<User> listUserTb = authorityService.getListUserTBANDPTB(listTb, textSearch, orgId);
        List<User> listUserPb = authorityService.getListUserTBANDPTB(listPb, textSearch, orgId);
        listUserLDC.addAll(listUserTb);
        listUserLDC.addAll(listUserPb);
        listUserLDC.addAll(usersData);
        return listUserLDC.stream().distinct().collect(Collectors.toList());
    }

    public Page<UserInfoDto> allUserInOrg(Long orgId, String textSearch, Boolean active, Pageable pageable) {
        List<Long> orgIds = orgService.orgAndSub(orgId);
        return userRepository.findUserByOrgInAndClientIdAndActive(orgIds, textSearch, BussinessCommon.getClientId(), active, pageable);
    }

    public List<User> getLeadByListUserId(List<Long> userIds, List<Long> exceptUserList) {
        return userRepository.getLeadByListUserId(userIds, exceptUserList, BussinessCommon.getClientId());
    }

    public List<User> getListNguoiUyQuyenVanThuBan() {
        List<User> listCvp = new ArrayList<>();
        Long idCVP = catService.findByName(BussinessCommon.getClientId(), Constant.CHANH_VAN_PHONG).getId();
        if (idCVP != null) {
            listCvp = userRepository.getListUserByPositionIdWithParentOrg(BussinessCommon.getClientId(), BussinessCommon.getOrgId(), idCVP);
            listCvp.add(BussinessCommon.getUser());
            return listCvp;
        }
        return listCvp;
    }

    public List<User> getListNguoiUyQuyen() {
        List<Long> listDF = delegateFlowService.listFromPosition();
        Set<Long> posIds = new HashSet<>(listDF);
        return userRepository.findListUserByOrgAndPositionIdInAndActive(BussinessCommon.getUser().getOrg(), posIds,
                true, BussinessCommon.getClientId());
    }

    public List<User> findListNguoiDuocUyQuyen(Long userId) {
        Optional<User> user = userRepository.findById(userId);
        if (!user.isPresent()) {
            throw new RestExceptionHandler(Message.NOT_FOUND_USER);
        }
        User u = user.get();
        List<Long> orgIds = orgService.orgAndSub(u.getOrg());
        return userRepository.findListNguoiDuocUyQuyen(u, orgIds, true);
    }

    public List<User> findByClientIdAndLDAP(long clientId, boolean b) {
        return userRepository.findByClientIdAndLDAP(clientId, b);
    }

    public boolean isAvatar(String fileName) {
        return userRepository.isAvatar(fileName);
    }

    public User validUs(Long id, List<User> userList) {
        Optional<User> opt = userList.stream().filter(i -> i.getId().equals(id)).findFirst();
        if (opt.isPresent()) {
            return opt.get();
        }
        throw new RestExceptionHandler(Message.INVALID_ACCOUNT);
    }

    public List<User> findUserBySerialNumberAndActive(String serialToken) {
        return userRepository.findUserBySerialNumber(serialToken, BussinessCommon.getClientId());
    }

    public boolean isSameClientId(List<Long> ids, Long clientId) {
        return userRepository.isSameClientId(ids, clientId);
    }

    @Transactional
    public void resetAll(String newPassword, Long clientId) {
        userRepository.reset(null, newPassword, clientId);
    }

    @Transactional
    public void resetList(List<Long> ids, String newPassword, Long clientId) {
        userRepository.reset(ids, newPassword, clientId);
    }

    public List<User> getListUserByOrgIdsAndUserIds(Long[] orgIdArr, Long[] userIdArr) {
        List<User> toUsers = new ArrayList<>();

        if (!BussinessCommon.isEmptyArr(orgIdArr)) {
            List<User> uLeadAndDefault = getListLeadByOrg(orgIdArr);
            if (uLeadAndDefault.isEmpty()) {
                throw new RestExceptionHandler(Message.SET_DEFAULT_NOT_YET);
            }
            toUsers.addAll(uLeadAndDefault);
        }

        if (!BussinessCommon.isEmptyArr(userIdArr)) {
            List<User> users = findByIds(Arrays.asList(userIdArr), true);
            if (users.size() != userIdArr.length) {
                throw new RestExceptionHandler(Message.INVALID_ACCOUNT);
            }
            toUsers.addAll(users);
        }

        return toUsers.stream().distinct().collect(Collectors.toList());
    }

    public List<Long> findUserIdByOrgWithAuthority(Long orgId, AuthorityEnum authority) {
        return userRepository.findUserIdByOrgWithAuthority(orgId, authority, BussinessCommon.getClientId());
    }

    public List<UserDto> findUserByOrgWithAuthority(Long orgId, AuthorityEnum authority) {
        return userRepository.findUserByOrgWithAuthority(orgId, authority, BussinessCommon.getClientId());
    }

    public List<Long> getListIdsVanThuVBDenByOrg(Long orgId) {
        return userRepository.getListVanThuByOrgIdAndCodeIn(clericalOrg, Arrays.asList(ModuleCodeEnum.DOC_OUT_LIST.getName()), orgId, true, BussinessCommon.getClientId());
    }

    public List<Long> getListVanThuByOrgIdAndCodeInRemoveVT(Long orgId) {
        return userRepository.getListVanThuByOrgIdAndCodeInRemoveVT(clericalOrg, Arrays.asList(ModuleCodeEnum.DOC_OUT_LIST.getName()), orgId, true, BussinessCommon.getClientId());
    }

    public List<User> getUserVanThuVBDenByOrg(Long orgId) {
        List<Long> userIds = userRepository.getListVanThuByOrgIdAndCodeIn(clericalOrg, Arrays.asList(ModuleCodeEnum.DOC_OUT_LIST.getName()), orgId, true, BussinessCommon.getClientId());
        return findByIds(userIds, true);
    }

    public boolean isVanThuVBDen(User user) {
        return userRepository.isVanThuByOrgIdAndCodeIn(clericalOrg, user.getId(), null, Arrays.asList(ModuleCodeEnum.DOC_OUT_LIST.getName()), true, user.getClientId());
    }

    public boolean isVanThuVBDenByOrg(User user, Long orgId) {
        return userRepository.isVanThuByOrgIdAndCodeIn(clericalOrg, user.getId(), orgId, Arrays.asList(ModuleCodeEnum.DOC_OUT_LIST.getName()), true, user.getClientId());
    }

    public List<Long> getListIdsVanThuVBDiByOrg(Long orgId) {
        return userRepository.getListVanThuByOrgIdAndCodeIn(clericalOrg, Arrays.asList(ModuleCodeEnum.DRAFT_ISSUED.getName()), orgId, true, BussinessCommon.getClientId());
    }

    public boolean isVanThuVBDi(User user) {
        return userRepository.isVanThuByOrgIdAndCodeIn(clericalOrg, user.getId(), null, Arrays.asList(ModuleCodeEnum.DRAFT_ISSUED.getName()), true, user.getClientId());
    }

    public boolean isVanThuVBDiByOrg(User user, Long orgId) {
        return userRepository.isVanThuByOrgIdAndCodeIn(clericalOrg, user.getId(), orgId, Arrays.asList(ModuleCodeEnum.DRAFT_ISSUED.getName()), true, user.getClientId());
    }

    public boolean isClerical(DocumentTypeEnum docType) {
        if (DocumentTypeEnum.VAN_BAN_DEN.equals(docType)) {
            return isVanThuVBDen(BussinessCommon.getUser());
        }
        return isVanThuVBDi(BussinessCommon.getUser());
    }

    public Page<UserInfoDto> findUserByOrgAndPositionAndFullName(Long orgId, Long posId, String fullName, Integer page) {
        return userRepository.findUserByOrgAndPositionAndFullNameAndActiveAndClientId(orgId, posId, fullName.toLowerCase(), true, BussinessCommon.getClientId(), BussinessCommon.castToPageable(page));
    }

    public Page<ClericalWithOrgIds> getClerical(String name, Long orgId, DocumentTypeEnum docType, Pageable page) {
        List<String> code;
        if (docType == null) {
            code = Arrays.asList(ModuleCodeEnum.DOC_OUT_LIST.getName(), ModuleCodeEnum.DRAFT_ISSUED.getName());
        } else if (DocumentTypeEnum.VAN_BAN_DEN.equals(docType)) {
            code = Arrays.asList(ModuleCodeEnum.DOC_OUT_LIST.getName());
        } else {
            code = Arrays.asList(ModuleCodeEnum.DRAFT_ISSUED.getName());
        }
        return userRepository.getListUserInfoDtoByModuleCodeAndOrgIdAndActiveAndClientId(BussinessCommon.convert(name), code, orgId, true, BussinessCommon.getClientId(), page);
    }

    public List<Long> findAllLeadershipInParentOrg(Long parentOrg) {
        return userRepository.findAllLeadershipInParentOrg(parentOrg);
    }

    public String getPhone(Long userId) {
        return userRepository.getPhone(userId);
    }

    public List<UserBasicDto> findLeadershipByOrgId(Long orgId) {
        return userRepository.findAllLeadershipByOrgIdAndClientId(orgId, BussinessCommon.getClientId());
    }

    public List<UserBasicDto> findLanhDaoKy() {
        User u = BussinessCommon.getUser();
        Long orgId = u.getOrg();
        if (u.getOrgModel().getParentId() != null) {
            orgId = u.getOrgModel().getParentId();
        }
        return userRepository.findAllLeadershipByOrgIdAndClientId(orgId, BussinessCommon.getClientId());
    }

    public List<Long> findLeadershipByOrgIdIn(List<Long> orgIds) {
        return userRepository.findLeadershipByOrgIdIn(orgIds, BussinessCommon.getClientId());
    }

    public List<Long> findAllLeaderByOrgIdAndClientId(List<Long> orgIds) {
        return userRepository.findLeaderByOrgIdIn(orgIds, BussinessCommon.getClientId());
    }

    public UserFullNamePositionName fullNamePositionName(Long userId) {
        return userRepository.fullNamePositionName(userId);
    }

    public List<String> exist(List<String> userNameList, Long ldapClientId) {
        return userRepository.exist(userNameList, ldapClientId);
    }

    @Transactional
    public void setLdap(List<String> existUser, Long ldapClientId) {
        userRepository.setLdap(existUser, ldapClientId);
    }

    public List<Long> findByOrgIds(List<Long> orgIds) {
        return userRepository.findByClientIdAndOrgInAndActiveTrue(BussinessCommon.getClientId(), orgIds);
    }

    public List<UserInfoDto> findUserByOrgAndChildWithAuthority(Long orgId, AuthorityEnum authority) {
        List<Long> orgSub = orgService.orgAndSub(orgId);
        return userRepository.findUserByListOrgWithAuthority(orgSub, authority, BussinessCommon.getClientId());
    }

    public List<UserInfoDto> findUserByOrgAndChildWithoutAuthority(Long orgId) {
        List<Long> orgSub = orgService.orgAndSub(orgId);
        return userRepository.findUserByListOrgWithoutAuthority(orgSub, BussinessCommon.getClientId());
    }

    public List<UserInfoDto> allUserInOrgs(Long orgId, String textSearch, Boolean active) {
        Organization organization = orgService.getOrgCucByOrg(BussinessCommon.getUser().getOrgModel());
        List<Long> orgIds = orgService.orgAndSub(organization.getId());
        return userRepository.findUserByOrgInAndClientIdAndActives(orgIds, textSearch, BussinessCommon.getClientId(), active);
    }

    public List<SearchNameDto> searchName(String name) {
        return userRepository.searchName(name, BussinessCommon.getClientId());
    }

    public List<UserBasicDto> findBasicInfoByOrgId(Long orgId, Boolean isLead) {
        return userRepository.findBasicInfoByOrgId(orgId, isLead, BussinessCommon.getClientId());
    }

    private ROLE getRole() {
        User user = BussinessCommon.getUser();
        boolean lanhDaoPhong = authorityService.isUserHasAuthority(user.getId(), null, AuthorityEnum.LEADERSHIP_UNIT);
        boolean lanhDaoCuc = authorityService.isUserHasAuthority(user.getId(), null, AuthorityEnum.LEADERSHIP);

        boolean tkPhong = Constant.PHONG.equalsIgnoreCase(user.getOrgModel().getOrgTypeModel().getName());
        boolean tkCuc = Constant.CUC_VU_VIEN.equalsIgnoreCase(user.getOrgModel().getOrgTypeModel().getName());
        boolean tkBan = Constant.BAN.equalsIgnoreCase(user.getOrgModel().getOrgTypeModel().getName());

        boolean lanhDaoCapPhong = tkPhong && lanhDaoPhong;
        boolean lanhDaoCapCuc = tkCuc && lanhDaoCuc;
        boolean lanhDaoCapBan = tkBan && user.isLead();

        ROLE role;
        if (lanhDaoCapPhong) {
            role = ROLE.LANH_DAO_CAP_PHONG;
        } else if (lanhDaoCapCuc) {
            role = ROLE.LANH_DAO_CAP_CUC;
        } else if (lanhDaoCapBan) {
            role = ROLE.LANH_DAO_CAP_BAN;
        } else {
            role = ROLE.CAN_BO;
        }

        return role;
    }

    private List<Long> getOrgByRole(ROLE role) {
        User user = BussinessCommon.getUser();
        Long orgId = user.getOrg();
        List<Long> orgIds = new ArrayList<>();

        Long tmp;
        switch (role) {
            case CAN_BO:
                orgIds.add(orgId);
                break;
            case LANH_DAO_CAP_PHONG:
                tmp = orgService.getParentByOrgType(user.getOrgModel(), Constant.CUC_VU_VIEN);
                orgIds = orgService.orgAndSub(tmp);
                break;
            case LANH_DAO_CAP_CUC:
                tmp = orgService.getParentByOrgType(user.getOrgModel(), Constant.BAN);
                orgIds = orgService.orgAndSub(tmp);
                break;
            case LANH_DAO_CAP_BAN:
                tmp = orgId;
                orgIds = orgService.orgAndSub(tmp);
                break;
            default:
                orgIds.add(orgId);
                break;
        }

        return orgIds;
    }

    private enum ROLE {
        CAN_BO, LANH_DAO_CAP_PHONG, LANH_DAO_CAP_CUC, LANH_DAO_CAP_BAN
    }

    public List<OrganizationBasicDto> getTree() {
        ROLE role = getRole();
        List<Long> orgIds = getOrgByRole(role);
        List<OrganizationBasicDto> rs = convert(orgIds, role);
        return getTree(rs, role);
    }

    private List<OrganizationBasicDto> convert(List<Long> orgIds, ROLE role) {
        if (orgIds.isEmpty()) {
            return new ArrayList<>();
        }

        List<OrganizationBasicDto> orgs = new ArrayList<>();
        List<Organization> oList = orgService.getOrgByIdList(orgIds, true);

        OrganizationBasicDto tmp;
        List<UserBasicDto> uList;
        for (Organization i : oList) {
            if (ROLE.LANH_DAO_CAP_BAN.equals(role)
                    && (Constant.PHONG.equalsIgnoreCase(i.getOrgTypeModel().getName()))) {
                continue;
            }

            uList = findBasicInfoByOrgId(i.getId(),
                    i.getId().equals(BussinessCommon.getOrgId()) ? null : Boolean.TRUE);
            removeCurUser(uList);

            tmp = new OrganizationBasicDto(i.getId(), i.getName(), i.getParentId(), i.getOrgTypeModel().getName());
            tmp.setUsers(uList); // if is current org -> user is get all
            tmp.setOrder(i.getId().equals(BussinessCommon.getOrgId()) ? 1 : 0);
            orgs.add(tmp);
        }

        orgs.sort(Comparator.comparing(OrganizationBasicDto::getOrder).reversed());
        return orgs;
    }

    /*
     * remove current user
     */
    private void removeCurUser(List<UserBasicDto> uList) {
        uList.removeIf(i -> i.getId().equals(BussinessCommon.getUserId()));
    }

    private List<OrganizationBasicDto> getTree(List<OrganizationBasicDto> all, ROLE role) {
        if (BussinessCommon.isEmptyList(all)) {
            return new ArrayList<>();
        }

        List<OrganizationBasicDto> root = new ArrayList<>();
        if (ROLE.LANH_DAO_CAP_BAN.equals(role)) {
            root = all.stream().filter(i -> i.getParentId() == null).collect(Collectors.toList());
        } else if (ROLE.LANH_DAO_CAP_CUC.equals(role) || ROLE.LANH_DAO_CAP_PHONG.equals(role)) {
            root = all.stream().filter(i -> Constant.CUC_VU_VIEN.equals(i.getOrgTypeModelName()))
                    .collect(Collectors.toList());
        }

        if (root.isEmpty()) {
            return all;
        }

        buildTree(root, all);
        return root;
    }

    private void buildTree(List<OrganizationBasicDto> list, List<OrganizationBasicDto> all) {
        if (list.isEmpty() || all.isEmpty()) {
            return;
        }

        for (OrganizationBasicDto i : list) {
            setChilds(i, all);
            buildTree(i.getChildren(), all);
        }
    }

    private void setChilds(OrganizationBasicDto p, List<OrganizationBasicDto> all) {
        if (all.isEmpty()) {
            return;
        }

        for (OrganizationBasicDto i : all) {
            if (p.getId().equals(i.getParentId())) {
                p.getChildren().add(i);
            }
        }
        all.removeIf(i -> i.getId().equals(p.getId()));
    }

    public List<LabelValueId<String>> getCertAndFullNameByUserId(List<Long> userIds, Boolean cert) {
        return userRepository.getCertAndFullNameByUserId(userIds, cert, BussinessCommon.getClientId());
    }

    public List<LabelValueId<String>> getCertAndFullNameByUserIdManager(List<Long> userIds, Boolean cert) {
        return userRepository.getCertAndFullNameByUserIdManager(userIds, cert, BussinessCommon.getClientId());
    }

    public void validCertByUserId(List<LabelValueId<String>> rs) {
        if (BussinessCommon.isEmptyList(rs)) {
            return;
        }

        rs.forEach(i -> {
            if (StringUtils.isNullOrEmpty(i.getValue())) {
                throw new RestExceptionHandler(Message.ENCRYPTED_FILE_INVALID);
            }
        });
    }

    public List<Long> findUserIdByOrgId(Long orgId) {
        List<User> users = findByOrgIdAndClientId(orgId, BussinessCommon.getClientId());
        List<Long> ids = users.stream().map(User::getId).collect(Collectors.toList());
        return ids;
    }

    public List<Long> findUserIdByFullName(String[] fullNames) {
        List<Long> rs = new ArrayList<>();
        if (ArrayUtils.isEmpty(fullNames))
            return rs;
        fullNames = Arrays.stream(fullNames).map(String::trim).toArray(String[]::new);
        List<Long> ids = userRepository.findUserIdByFullName(fullNames, BussinessCommon.getClientId());
        return ids;
    }

    public List<User> getListLeadUserByOrg(List<Long> orgIds) {
        if (BussinessCommon.isEmptyList(orgIds)) {
            return Collections.emptyList();
        }
        Long[] tmpArr = orgIds.stream().toArray(Long[]::new);
        return getListLeadByOrg(tmpArr);
    }

    /**
     * Get map org id - lead user id
     *
     * @param orgIds
     * @return
     */
    public Map<Long, Long> leadByOrgIdsMap(List<Long> orgIds) {
        Map<Long, Long> map = new HashMap<>();
        if (BussinessCommon.isEmptyList(orgIds)) {
            return map;
        }
        List<User> uList = getListLeadUserByOrg(orgIds);
        for (User user : uList) {
            Long key = user.getOrg();
            Long value = user.getId();
            if (!map.containsKey(key)) {
                map.put(key, value);
            }
        }
        return map;
    }

    public User getByEmail(String email) {
        return userRepository.findFirstByEmailAndActiveTrue(email);
    }

    public User getBySerialToken(String serialToken) {
        return userRepository.findFirstBySerialToken(serialToken);
    }

    public Map<String, String> checkTokenExits(String serialToken) {
        Map<String, String> map = new HashMap<>();
        List<User> userNameToken = userRepository.findByTokenAndClientId(BussinessCommon.getClientId(), serialToken);
        if (userNameToken.size() > 0) {
            map.put("userName", userNameToken.get(0).getFullName());
        } else {
            map.put("userName", null);
        }
        return map;
    }

    public void removeTokenExits(String serialToken) {
        Map<String, String> map = new HashMap<>();
        List<User> userNameToken = userRepository.findByTokenAndClientId(BussinessCommon.getClientId(), serialToken);
        for (User user : userNameToken) {
            user.setSerialToken("");
            user.setNameToken("");
            user.setStartTimeToken("");
            user.setExpiredTimeToken("");
            userRepository.save(user);
        }
    }

    public Map<String, String> checkCertExits(String cert) {
        Map<String, String> map = new HashMap<>();
        List<User> userNameCert = userRepository.findByCertAndClientId(BussinessCommon.getClientId(), cert);
        if (userNameCert.size() > 0) {
            map.put("userName", userNameCert.get(0).getFullName());
        } else {
            map.put("userName", null);
        }
        return map;
    }

    public void removeCertExits(String cert) {
        Map<String, String> map = new HashMap<>();
        List<User> userNameCert = userRepository.findByCertAndClientId(BussinessCommon.getClientId(), cert);
        for (User user : userNameCert) {
            user.setCert("");
            userRepository.save(user);
        }
    }

    /**
     * Update when user forget password
     *
     * @param user
     * @return
     */
    public User changeRandomPassword(String newPw, User user) {
        if (user == null) {
            throw new RestExceptionHandler(Message.INVALID_ACCOUNT);
        }

        PasswordEncoder encoder = new BCryptPasswordEncoder();
        user.setPassword(encoder.encode(newPw));
        user.setChangePass(true);
        user.setLastLogin(new Date());
        user.setForgetPassword(true);
        return save(user);
    }

    /**
     * Handle forget password
     *
     * @param email
     * @return
     */
    public Boolean forgetPasword(String email) {
        User user = getByEmail(email);
        if (!BussinessCommon.isEmail(email)) {
            throw new RestExceptionHandler(Message.EMAIL_INVALID);
        }

        if (user == null || user.isLdap()) {
            throw new RestExceptionHandler(Message.INVALID_ACCOUNT);
        }

        String newPw = PasswordUtils.autoGenPw();

        // send mail
        MailNotiDto dto = MailNotiDto.builder()
                .preview("Mật khẩu mới : " + newPw)
                .position(user.getPositionModel().getName())
                .fullName(user.getFullName())
                .docType("Quên mật khẩu")
                .email(email).build();
        try {
            mailNotiService.sendNoti(dto);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RestExceptionHandler(Message.MAIL_SYSTEM_ERROR);
        }

        // update password
        changeRandomPassword(newPw, user);
        return true;
    }

    private Role setAdminRole(boolean cabinet) {
        String roleName;
        Role role = new Role();
        if (Boolean.FALSE.equals(cabinet)) {
            roleName = Constant.OFFICE_ROLE_ADMIN;
            role = roleRepository.findByNameOfficeSite(roleName);
        } else {
            roleName = Constant.CABINET_ROLE_ADMIN;
            role = roleRepository.findByNameAndCabinetTrue(roleName);
        }
        if (role == null) {
            role = new Role(roleName, ldapClientId, ldapUpdateBy, cabinet);
        }

        if (Boolean.FALSE.equals(role.getActive())) {
            role.setActive(true);
        }
        return roleRepository.save(role);
    }

    private void checkAdminPermission(Long moduleId, Long roleId) {
        Permission permission = permissionRepository.findByModuleIdAndRoleId(moduleId, roleId);
        if (permission == null) {
            permission = new Permission(moduleId, roleId, ldapClientId, ldapUpdateBy);
        }
        if (Boolean.FALSE.equals(permission.getActive())) {
            permission.setActive(true);
        }
        permissionRepository.save(permission);
    }

    private void addAdminPermission(Module module, boolean cabinet) {
        if (module == null)
            return;

        Optional<Module> parent = moduleRepository.findById(module.getParentId());
        if (!parent.isPresent())
            return;
        Module moduleParent = parent.get();

        Role role = setAdminRole(cabinet);

        checkAdminPermission(module.getId(), role.getId());
        checkAdminPermission(moduleParent.getId(), role.getId());
    }

    private void setAdminPermission(boolean cabinet) {
        Module module = new Module();
        module = moduleRepository.findByCodeOfficeSite(Constant.OFFICE_ADMIN_ROLE);

        if (Boolean.TRUE.equals(cabinet)) {
            Module moduleCabinet = moduleRepository.findByCodeAndSite(Constant.CABINET_ADMIN_ROLES, SystemEnum.CABINET);
            addAdminPermission(moduleCabinet, cabinet);
        }

        addAdminPermission(module, cabinet);
    }

    private void setAdminUserRole(User admin, boolean cabinet) {
        Role role = setAdminRole(cabinet);

        UserRole ur = userRoleRepository.findByUserIdAndRoleId(admin.getId(), role.getId());

        if (ur == null) {
            ur = new UserRole(admin.getId(), role.getId(), ldapClientId, ldapUpdateBy);
        }
        if (Boolean.FALSE.equals(ur.getActive())) {
            ur.setActive(true);
        }

        userRoleRepository.save(ur);
    }

    public Boolean createAdmin() {
        String userName = "admin";
        String fullname = "Administrator";
        User admin = userRepository.findFirstByUserName(userName);
        if (admin == null) {
            PasswordEncoder encoder = new BCryptPasswordEncoder();
            admin = new User(fullname, userName, encoder.encode(Constant.PASSWORD_DEFAULT), ldapClientId, ldapOrgId, ldapUpdateBy);
        }
        if (Boolean.FALSE.equals(admin.getActive())) {
            admin.setActive(true);
        }

        userRepository.save(admin);
        setAdminPermission(false);
        setAdminUserRole(admin, false);
        setAdminPermission(true);
        setAdminUserRole(admin, true);
        return true;
    }

    /**
     * Target: add user when login LDAP success
     *
     * @param userName
     */
    public User addUserViaLdap(String userName) {
        User user = userRepository.findFirstByUserName(userName);
        if (user == null) {
            user = new User();
            user.setClientId(ldapClientId);
            user.setUserName(userName);
            user.setFullName(userName);
            user.setEmail(userName + ldapMail);
            user.setOrg(ldapOrgId);
            user.setPosition(ldapPositionId);
        }
        user.setActive(true);
        user.setLdap(true);
        user.setLastLogin(new Date());
        return add(user);
    }

    public List<Long> findUserIdByOrgIds(List<Long> orgIds) {
        return userRepository.findUserIdByOrgIds(orgIds, BussinessCommon.getClientId());
    }

    /**
     * Target: add user when login Connect Outside success
     *
     * @param userName
     */
    public User addUserViaConnectOutside() {
        String userName = "jack";
        User user = userRepository.findFirstByUserName(userName);
        if (user == null) {
            user = new User();
            user.setClientId(ldapClientId);
            user.setUserName(userName);
            user.setFullName(userName);
            user.setEmail(userName + ldapMail);
            user.setOrg(ldapOrgId);
            user.setPosition(ldapPositionId);
        }
        user.setActive(true);
        user.setLastLogin(new Date());
        return add(user);
    }

    /**
     * Valid users by connect outside
     *
     * @param userIds
     */
    public void validUserIdsByOutside(List<Long> userIds) {
        if (BussinessCommon.isEmptyList(userIds))
            throw new RestExceptionHandler(Message.INVALID_DATA_CONNECT_SYSTEM);
        long count = userRepository.validUserIdsByOutside(userIds, BussinessCommon.getClientId());
        if (count != userIds.size())
            throw new RestExceptionHandler(Message.INVALID_DATA_CONNECT_SYSTEM);
    }

    /**
     * Get id user list by full name list
     *
     * @param fullNames
     * @return
     */
    public List<Long> getIdsByFullNames(String[] fullNames) {
        if (fullNames == null || fullNames.length < 0)
            return Collections.emptyList();

        List<String> list = new ArrayList<>();
        for (String i : fullNames) {
            list.add(i.trim());
        }
        return userRepository.getIdsByFullNames(list, BussinessCommon.getClientId());
    }

    public List<Long> getListUserIdByOrgAndGroup(RequestCerOrgAndGroup requestCerOrgAndGroup) {
        Set<Long> rs = new HashSet<Long>();
        List<Long> listUserId = new ArrayList<>();
        if (requestCerOrgAndGroup.getUserIds() != null) rs.addAll(requestCerOrgAndGroup.getUserIds());
        if (requestCerOrgAndGroup.getParticipantsGroup() != null) {
            for (OrgGroupDto groupDto : requestCerOrgAndGroup.getParticipantsGroup()) {
                rs.addAll(userRepository.getUserIdByGroupId(groupDto.getId(), BussinessCommon.getClientId()));
            }
        }
        if (requestCerOrgAndGroup.getParticipantsOrg() != null) {
            for (OrgGroupDto orgDto : requestCerOrgAndGroup.getParticipantsOrg()) {
                rs.addAll(userRepository.getUserIdByGroupId(orgDto.getId(), BussinessCommon.getClientId()));
            }
        }
        listUserId.addAll(rs);
        return userRepository.getCertByUserId(listUserId, false, BussinessCommon.getClientId());
    }

    public Page<UserInfoDto> findUserByOrgAndPositionUsingGroupUser(List<Long> orgId, Long posId, String fullName, Integer page) {
        if (orgId == null || orgId.size() == 0)
            orgId = orgService.orgAndSub(BussinessCommon.getUser().getOrg());
        return userRepository.findUserByOrgAndPositionUsingGroupUser(orgId, posId, fullName.toLowerCase(), true, BussinessCommon.getClientId(), BussinessCommon.castToPageable(page));
    }

    public boolean checkTokenOfUser(String serialToken) {
        return serialToken.equals(BussinessCommon.getUser().getCert());
    }

    public List<Category> addAdditionalPositions(Long userId, List<Integer> positionIds) {
        Optional<User> userOptional = userRepository.findById(userId);
        if (userOptional.isPresent()) {
            User user = userOptional.get();
            for (Integer positionId : positionIds) {
                Long pId = Long.valueOf(positionId);
                Optional<UserCategory> userCategoryOptional = userCategoryRepository.findByUserIdAndCategoryId(userId, pId);
                UserCategory userCategory;
                if (userCategoryOptional.isPresent()) {
                    userCategory = userCategoryOptional.get();
                    userCategory.setActive(true);
                    userCategory.setUpdateDate(Date.from(Instant.now()));
                    userCategory.setUpdateBy(BussinessCommon.getUserId());
                } else {
                    userCategory = new UserCategory();
                    userCategory.setUserId(userId);
                    userCategory.setCategoryId(pId);
                    userCategory.setActive(true);
                    userCategory.setClientId(BussinessCommon.getClientId());
                    userCategory.setCreateBy(BussinessCommon.getUserId());
                    userCategory.setCreateDate(Date.from(Instant.now()));
                }
                userCategoryRepository.save(userCategory);
            }
        }
        return findCategoryByUserIdAndActive(userId, true, BussinessCommon.getClientId(), null);
    }

    public List<Category> removeAdditionalPositions(Long userId, List<Integer> positionIds) {

        Optional<User> userOptional = userRepository.findById(userId);
        if (userOptional.isPresent()) {
            for (Integer positionId : positionIds) {
                Long pId = Long.valueOf(positionId);
                Optional<UserCategory> userCategoryOptional = userCategoryRepository.findByUserIdAndCategoryId(userId, pId);
                if (userCategoryOptional.isPresent() && userCategoryOptional.get().getActive()) {
                    UserCategory userCategory = userCategoryOptional.get();
                    userCategory.setActive(false);
                    userCategory.setUpdateDate(Date.from(Instant.now()));
                    userCategory.setUpdateBy(BussinessCommon.getUserId());
                    userCategoryRepository.save(userCategory);
                }
            }
        }
        return findCategoryByUserIdAndActive(userId, true, BussinessCommon.getClientId(), null);
    }

    public Page<UserBasicDto> search(Long orgId, String text, Integer page) {
        text = BussinessCommon.convert(text);
        Pageable pageable = BussinessCommon.castToPageable(page);
        return userRepository.search(orgId, text, BussinessCommon.getClientId(), pageable);
    }

    public void linkMultipleOrgAndPosition(User input, Long userId, Boolean isUpdate) {
        if (isUpdate) {
            List<UserOrganization> userOrganizationList = userOrganizationRepository.findByClientIdAndUserIdAndActiveTrue(BussinessCommon.getClientId(), userId);
            userOrganizationRepository.deleteAll(userOrganizationList);
            List<UserCategory> userCategoryList = userCategoryRepository.findCategoryByUserId(BussinessCommon.getClientId(), userId);
            userCategoryRepository.deleteAll(userCategoryList);
        }
        if (input.getAdditionalOrganizations() != null) {
            for (Organization org : input.getAdditionalOrganizations()) {
                if (org.getPositions() != null) {
                    UserOrganization userOrganization = new UserOrganization();
                    userOrganization.setUserId(userId);
                    userOrganization.setOrgId(org.getId());
                    UserOrganization tmpUserOrg = userOrganizationRepository.save(userOrganization);
                    for (Category position : org.getPositions()) {
                        UserCategory userCategory = new UserCategory();
                        userCategory.setUserId(userId);
                        userCategory.setCategoryId(position.getId());
                        userCategory.setUserOrganizationId(tmpUserOrg.getId());
                        userCategoryRepository.save(userCategory);
                    }
                } else {
                    throw new RestExceptionHandler(Message.NOT_FOUND_POSITION + " của đơn vị: " + org.getName());
                }
            }
        } else {
            throw new RestExceptionHandler(Message.NOT_FOUND_ORG);
        }
    }
}
