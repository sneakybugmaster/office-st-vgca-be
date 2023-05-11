package com.vz.backend.core.controller;

import java.util.*;
import java.util.stream.Collectors;

import javax.websocket.server.PathParam;

import com.vz.backend.core.domain.*;
import com.vz.backend.core.domain.Module;
import com.vz.backend.core.dto.*;
import com.vz.backend.core.repository.IUserRepository;
import com.vz.backend.core.service.*;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.format.annotation.DateTimeFormat.ISO;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.core.auth.AuthenResponse;
import com.vz.backend.core.auth.SecurityContext;
import com.vz.backend.core.auth.TokenHelper;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.ActionEnum;
import com.vz.backend.core.config.AuthorityEnum;
import com.vz.backend.core.config.CategoryEnum;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.config.SystemEnum;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.exception.RestForbidden;
import com.vz.backend.core.repository.IClientRepository;
import com.vz.backend.core.thread.LdapConnection;
import com.vz.backend.util.DateTimeUtils;
import com.vz.backend.util.StringUtils;

import lombok.extern.slf4j.Slf4j;

/**
 * @author DucND
 * @date Apr 14, 2020
 */
@Slf4j
@RestController
@RequestMapping("/users")
public class AuthenController extends BaseController<User> {

    enum SortBy {
        UPDATEDATE("updateDate"), // ngày cập nhật
        FULLNAME("fullName"), // ngày cập nhật
        ORG("orgModel.name"), // ngày tạo
        POSITION("positionModel.name"), BIRTHDAY("birthday"), // số-kí hiệu
        PHONE("phone"), // trích yếu
        NAME_TOKEN("nameToken"),
        SERIAL_TOKEN("serialToken"),
        ACTIVE("active"); // ngày ban hành

        private String field;

        private SortBy(String field) {
            this.field = field;
        }
    }

    @Value("${hldap.clientId:1}")
    private Long ldapClientId;

    @Value("${configs.token.refresh-expire:86400000}")
    private long timeExpireRefreshToken;

    @Autowired
    AuthorityUserService authoriryService;

    @Autowired
    private StraceSystemService straceService;

    @Autowired
    private UserService userService;

    @Autowired
    private RoleService roleService;

    @Autowired
    private TokenHelper tokenProvider;

    @Autowired
    private IClientRepository clientRepository;

    @Autowired
    private ApplicationContext context;

    @Override
    public IService<User> getService() {
        return userService;
    }

    @Autowired
    private ModuleService moduleService;

    @Autowired
    OrganizationService orgService;

    @Autowired
    CategoryService catService;

    @Autowired
    MailNotiService mailNotiService;

    @Autowired
    SecretaryService secretaryService;

    @Autowired
    MapUserIAMService mapUserIAMService;

    @Autowired
    private IUserRepository userRepository;

    private void checkPermission() {
        if (roleService.existUserInModule(ModuleCodeEnum.USER.getName())) {
            return;
        }
        throw new RestForbidden("Bạn không có quyền truy cập vào người dùng");
    }

    // true : admin
    // false : user
    private boolean isAdmin(Long id) {
        if (roleService.existUserInModule(ModuleCodeEnum.USER.getName())) {
            return true;
        }
        if (BussinessCommon.getUserId().equals(id)) {
            return false;
        }

        throw new RestForbidden("Bạn không có quyền truy cập vào người dùng");
    }

    @PostMapping(value = "/search")
    public ResponseEntity<?> search(@RequestParam("textSearch") String textSearch) {
        return new ResponseEntity<>(userService.searchUserActive(textSearch), HttpStatus.OK);
    }

    @PostMapping(value = "/search1")
    public ResponseEntity<?> search1(@RequestParam("textSearch") String textSearch) {
        return new ResponseEntity<>(userService.searchUserDtoActive(textSearch), HttpStatus.OK);
    }

    @GetMapping("/search")
    public ResponseEntity<Page<UserBasicDto>> search(@RequestParam(required = false) Long orgId,
                                                     @RequestParam(required = false) String text, @RequestParam(required = false, defaultValue = "1") Integer page) {
        return new ResponseEntity<>(userService.search(orgId, text, page), HttpStatus.OK);
    }

    @GetMapping(value = "/search-name")
    public ResponseEntity<List<SearchNameDto>> searchName(@RequestParam("q") String q) {
        List<SearchNameDto> result = new ArrayList<>();
        q = StringUtils.handleSubmit(q);
        List<UserInfoDto> listUser = userService.allUserInOrgs(BussinessCommon.getOrgId(), q, true);
        if (q != null && !q.isEmpty() && !listUser.isEmpty()) {
            for (UserInfoDto userDto : listUser) {
                SearchNameDto userResult = new SearchNameDto(userDto.getFullName(), userDto.getPositionName());
                result.add(userResult);
            }
        } else {
            result = null;
        }
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/version")
    public ResponseEntity<ResponseMessage> version() {
        return new ResponseEntity<>(new ResponseMessage("Version BE: 1280"), HttpStatus.OK);
    }
    //	@GetMapping(value = "/get-client-token")
    //	public ResponseEntity<?> getClientToken() {
    //		return new ResponseEntity<>(genClientToken(), HttpStatus.OK);
    //	}

    private HashMap<String, String> genClientToken() {
        String tokenH05 = tokenProvider.generateExpire(DateTimeUtils.setDate(Constant.DEFAULT_EXPIRED_START_DATE_H05),
                Constant.TOKEN_H05);
        String tokenH03 = tokenProvider.generateExpire(DateTimeUtils.setDate(Constant.DEFAULT_EXPIRED_START_DATE_H03),
                Constant.TOKEN_H03);
        String tokenBCY = tokenProvider.generateExpire(DateTimeUtils.setDate(Constant.DEFAULT_EXPIRED_START_DATE_BCY),
                Constant.TOKEN_BCY);

        log.info(Constant.TOKEN_H03 + ": " + tokenH03);
        log.info(Constant.TOKEN_H05 + ": " + tokenH05);
        log.info(Constant.TOKEN_BCY + ": " + tokenBCY);

        HashMap<String, String> hashMap = new HashMap<>();
        hashMap.put(Constant.TOKEN_H03, tokenH03);
        hashMap.put(Constant.TOKEN_H05, tokenH05);
        hashMap.put(Constant.TOKEN_BCY, tokenBCY);
        return hashMap;
    }

    @GetMapping("/login/cabinet")
    public ResponseEntity<?> loginCabinet() {
        User userInfo = BussinessCommon.getUser();
        Token tokenInfo = tokenProvider.generateToken(userInfo.getUserName());
        List<Module> moduleAll = getAllModuleCabinet(userInfo);
        userInfo.setAuthoritys(authoriryService.get(userInfo.getId(), userInfo.getClientId(), true));
        userInfo.setCecretarys(secretaryService.findSecretaryByBossId(userInfo.getId(), userInfo.getClientId(), true));
        userInfo = userService.setAllOrgAndPositionUser(userInfo);
        AuthenResponse response = new AuthenResponse(tokenInfo, userInfo, moduleAll);

        userInfo.setLastLogin(new Date());
        userService.save(userInfo);
        straceService.save(userInfo.getId(), ActionEnum.LOGIN.getName(), userInfo.getFullName(),
                CategoryEnum.LOGIN.getValue(), userInfo);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @PostMapping("/login")
    public ResponseEntity<?> authentication(@RequestParam String userName, @RequestParam String password,
                                            @RequestParam(required = false, defaultValue = "FALSE") Boolean rememberPassword) {
        // valid
        if (StringUtils.isNullOrEmpty(userName) || StringUtils.isNullOrEmpty(password)) {
            throw new RestExceptionHandler(Message.NO_INPUT_DATA);
        }

        User userInfo = userService.findByUserNameAndActive(userName, true);
        if (userInfo != null && !Boolean.TRUE.equals(userInfo.isLdap())) {
            log.info("Login without ldap");
            PasswordEncoder encoder = new BCryptPasswordEncoder();
            if (!encoder.matches(password, userInfo.getPassword())) {
                throw new RestExceptionHandler(Message.FAIL_LOGIN);
            }
        } else {
            log.info("Login with ldap");
            LdapConnection ldapConnection = context.getBean(LdapConnection.class);

            log.info("Ldap is connecting...");
            if (!ldapConnection.loginLdap(userName, password)) {
                throw new RestExceptionHandler(Message.FAIL_LOGIN);
            }

            userInfo = userService.addUserViaLdap(userName);
        }

        checkExpireClient(userInfo);

        Token tokenInfo = tokenProvider.generateToken(userInfo.getUserName());
        List<Module> moduleAll = getAllModule(userInfo);
        userInfo.setAuthoritys(authoriryService.get(userInfo.getId(), userInfo.getClientId(), true));
        userInfo.setCecretarys(secretaryService.findSecretaryByBossId(userInfo.getId(), userInfo.getClientId(), true));
        userInfo = userService.setAllOrgAndPositionUser(userInfo);
        AuthenResponse response = new AuthenResponse(tokenInfo, userInfo, moduleAll);
        userInfo.setRememberPassword(Boolean.TRUE.equals(rememberPassword));
        userInfo.setLastLogin(new Date());
        userService.save(userInfo);
        straceService.save(userInfo.getId(), ActionEnum.LOGIN.getName(), userName, CategoryEnum.LOGIN.getValue(),
                userInfo);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    private void checkExpireClient(User userInfo) {
        Optional<Client> clientOpt = clientRepository.findById(userInfo.getClientId());
        if (!clientOpt.isPresent()) {
            throw new RestExceptionHandler(Message.FAIL_LOGIN);
        }

        Client clientInfo = clientOpt.get();
        //		if (!tokenProvider.checkExpireClient(clientInfo.getToken())) {
        //			throw new RestExceptionHandler(Message.ERROR_SYS);
        //		}
    }

    @PostMapping("/login/cas")
    public ResponseEntity<?> authentication(@RequestParam String token) {
        String userName = tokenProvider.getUserIdFromJWT(token);
        User userInfo = userService.findByUserNameAndActive(userName, true);
        if (userInfo == null) {
            return new ResponseEntity<>(new ArrayList<>(), HttpStatus.UNAUTHORIZED);
        }

        checkExpireClient(userInfo);

        Token tokenInfo = tokenProvider.generateToken(userInfo.getUserName());
        List<Module> moduleAll = getAllModule(userInfo);
        userInfo.setAuthoritys(authoriryService.get(userInfo.getId(), userInfo.getClientId(), true));
        userInfo.setCecretarys(secretaryService.findSecretaryByBossId(userInfo.getId(), userInfo.getClientId(), true));
        AuthenResponse response = new AuthenResponse(tokenInfo, userInfo, moduleAll);

        userInfo.setLastLogin(new Date());
        userInfo = userService.setAllOrgAndPositionUser(userInfo);
        userService.save(userInfo);

        straceService.save(userInfo.getId(), ActionEnum.LOGIN.getName(), userName, CategoryEnum.LOGIN.getValue(),
                userInfo);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @PostMapping("/login/tk")
    public ResponseEntity<?> viaToken(@RequestParam String serialToken) {
        if (StringUtils.isNullOrEmpty(serialToken)) {
            throw new RestExceptionHandler("Chưa nhập token");
        }
        User userInfo = userService.findBySerialToken(serialToken);
        if (userInfo == null) {
            return new ResponseEntity<>(new ArrayList<>(), HttpStatus.UNAUTHORIZED);
        }

        String userName = userInfo.getUserName();
        checkExpireClient(userInfo);

        Token tokenInfo = tokenProvider.generateToken(userName);
        List<Module> moduleAll = getAllModule(userInfo);
        userInfo.setAuthoritys(authoriryService.get(userInfo.getId(), userInfo.getClientId(), true));
        userInfo.setCecretarys(secretaryService.findSecretaryByBossId(userInfo.getId(), userInfo.getClientId(), true));
        AuthenResponse response = new AuthenResponse(tokenInfo, userInfo, moduleAll);

        userInfo.setLastLogin(new Date());
        userInfo = userService.setAllOrgAndPositionUser(userInfo);
        userService.save(userInfo);

        straceService.save(userInfo.getId(), ActionEnum.LOGIN.getName(), userName, CategoryEnum.LOGIN.getValue(),
                userInfo);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    private List<Module> getAllModuleCabinet(User userInfo) {
        Long clientId = BussinessCommon.getClientId();
        List<Module> moduleOfUser = new ArrayList<>();
        List<Module> moduleOfUserClone = new ArrayList<>();
        List<Role> roleByUser = userService.findRoleByUserIdAndActive(userInfo, true, clientId, true);
        List<Category> categoryByUser = userService.findCategoryByUserIdAndActive(userInfo.getId(), true, clientId, userInfo.getOrg());
        userInfo.setRoles(roleByUser);
        if (userInfo.getDefaultRole() == null) {
            Role tmpRole = null;
            List<Role> tmpRoles = new ArrayList<>();
            if (!roleByUser.isEmpty()) {
                if (userInfo.getCurrentRole() != null) {
                    tmpRole = roleService.findByIdAndClientIdAndActiveTrue(userInfo.getCurrentRole(), clientId);
                }

                if (tmpRole == null) {
                    tmpRole = roleByUser.get(0);
                }

                userInfo.setCurrentRole(tmpRole.getId());
                tmpRoles.add(tmpRole);
                moduleOfUser = moduleService.findByRoleList(tmpRoles);
            } else {
                moduleOfUser = moduleService.findByRoleList(roleByUser);
                userInfo.setCurrentRole(0L);
            }
        } else {
            Role defaultRole = null;
            for (Role r : roleByUser) {
                if (r.getId().equals(userInfo.getCurrentRole() != null ? userInfo.getCurrentRole() : userInfo.getDefaultRole())) {
                    defaultRole = r;
                    break;
                }
            }
            if (defaultRole != null) {
                List<Role> lr = new ArrayList<>();
                lr.add(defaultRole);
                moduleOfUser = moduleService.findByRoleList(lr);
                userInfo.setCurrentRole(defaultRole.getId());
            } else {
                moduleOfUser = moduleService.findByRoleList(roleByUser);
                userInfo.setCurrentRole(0L);
            }
        }

        for (Module m : moduleOfUser) {
            if (SystemEnum.CABINET.equals(m.getSite())) {
                continue;
            }
            Module t = new Module();
            BeanUtils.copyProperties(m, t);
            moduleOfUserClone.add(t);
        }
        userInfo.setAuthorize(moduleOfUserClone);

        List<Module> modules = moduleService.findAdminModuleForAdminCabinet(userInfo.getClientId());
        return modules;
    }

    private List<Module> getAllModule(User userInfo) {
        List<Module> moduleOfUser = new ArrayList<>();
        List<Module> moduleOfUserClone = new ArrayList<>();
        List<Role> roleByUser = userService.findRoleByUserIdAndActive(userInfo, true, userInfo.getClientId(), null);
        List<Category> categoryByUser = userService.findCategoryByUserIdAndActive(userInfo.getId(), true, userInfo.getClientId(), userInfo.getOrg());
        userInfo.setRoles(roleByUser);

        // If the default role is set yet, the default role is the first role of roleByUser
        if (userInfo.getDefaultRole() == null) {
            Role tmpRole = null;
            List<Role> tmpRoles = new ArrayList<>();
            if (!roleByUser.isEmpty()) {
                tmpRole = roleByUser.get(0);
                userInfo.setCurrentRole(tmpRole.getId());
                tmpRoles.add(tmpRole);
                moduleOfUser = moduleService.findByRoleList(tmpRoles);
            } else {
                moduleOfUser = moduleService.findByRoleList(roleByUser);
                userInfo.setCurrentRole(0L);
            }
        } else {
            Role defaultRole = null;
            for (Role r : roleByUser) {
                if (r.getId().equals(userInfo.getDefaultRole())) {
                    defaultRole = r;
                    break;
                }
            }

            List<Role> lr = new ArrayList<>();
            if (defaultRole != null) {
                lr.add(defaultRole);
                userInfo.setCurrentRole(defaultRole.getId());
                moduleOfUser = moduleService.findByRoleList(lr);
            } else if (!roleByUser.isEmpty()) {
                lr.add(roleByUser.get(0));
                userInfo.setDefaultRole(roleByUser.get(0).getId());
                userInfo.setCurrentRole(roleByUser.get(0).getId());
                moduleOfUser = moduleService.findByRoleList(lr);
            } else {
                moduleOfUser = moduleService.findByRoleList(roleByUser);
                userInfo.setCurrentRole(0L);
            }
        }

        for (Module m : moduleOfUser) {
            Module t = new Module();
            BeanUtils.copyProperties(m, t);
            moduleOfUserClone.add(t);
        }
        userInfo.setAuthorize(moduleOfUserClone);

        List<Module> modules = moduleService.findByClientIdAndParentId(userInfo.getClientId());
        return modules;
    }

    @GetMapping("/getModules/{roleId}")
    public ResponseEntity<?> getModules(@PathVariable Long roleId) {
        Optional<Role> r = roleService.findById(roleId);
        if (r.isPresent()) {
            List<Role> lr = new ArrayList<>();
            lr.add(r.get());
            List<Module> moduleOfUser = moduleService.findByRoleList(lr);
            return new ResponseEntity<>(moduleOfUser, HttpStatus.OK);
        }
        return new ResponseEntity<>(new ArrayList<>(), HttpStatus.NOT_FOUND);
    }

    @PostMapping("/registry")
    public ResponseEntity<?> createUser(@RequestBody User user) {
        checkPermission();
        try {
            user.valids();
            Long clientId = SecurityContext.getCurrentUser().getClientId();
            User userInfo = userService.findByUserNameAndClientId(user.getUserName(), clientId);
            if (userInfo != null) {
                throw new RestExceptionHandler(Message.EXIST_USER_NAME);
            }

            // Check exist email
            if (user.getEmail() != null) {
                User tmpEmail = userService.getByEmail(user.getEmail());
                if (tmpEmail != null) {
                    throw new RestExceptionHandler(Message.DUPLICATE_EMAIL);
                }
            }

            // Check exist serial token
            if (!StringUtils.isNullOrEmpty(user.getSerialToken())) {
                User tmpSerialToken = userService.getBySerialToken(user.getSerialToken());
                if (tmpSerialToken != null) {
                    throw new RestExceptionHandler(Message.DUPLICATE_TOKEN);
                }
            }

            PasswordEncoder encoder = new BCryptPasswordEncoder();
            user.setPassword(encoder.encode(user.getPassword()));
            user.setChangePass(false);
            user.setActive(true);
            user.setClientId(clientId);
            return new ResponseEntity<>(userService.save(user), HttpStatus.OK);
        } catch (Exception e) {
            return ResponseEntity.badRequest().build();
        }
    }

    @PostMapping("/addUser")
    public ResponseEntity<?> addUser(@RequestBody User user) {
        checkPermission();
        user.valids();

        Long clientId = SecurityContext.getCurrentUser().getClientId();
        User userInfo = userService.findByUserNameAndClientId(user.getUserName(), clientId);
        if (userInfo != null) {
            throw new RestExceptionHandler(Message.EXIST_USER_NAME);
        }
        if (user.getOrg() == null) {
            throw new RestExceptionHandler(Message.NOT_FOUND_ORG);
        }

        // Check exist email
        if (user.getEmail() != null) {
            User tmp = userService.getByEmail(user.getEmail());
            if (tmp != null) {
                throw new RestExceptionHandler(Message.DUPLICATE_EMAIL);
            }
        }

        // Check exist serial token
        if (!StringUtils.isNullOrEmpty(user.getSerialToken())) {
            User tmpSerialToken = userService.getBySerialToken(user.getSerialToken());
            if (tmpSerialToken != null) {
                throw new RestExceptionHandler(Message.DUPLICATE_TOKEN);
            }
        }

        PasswordEncoder encoder = new BCryptPasswordEncoder();
        user.setPassword(encoder.encode(Constant.PASSWORD_DEFAULT));
        user.setChangePass(false);
        user.setActive(true);
        user.setClientId(clientId);
        user.setPositionModel(null);

        User tmp = userService.save(user);

        // set link multiple org and position
        userService.linkMultipleOrgAndPosition(user, tmp.getId(), false);

        // Phân công thư kí
        secretaryService.save(tmp.getId(), user.getCecretarys());

        return new ResponseEntity<>(tmp, HttpStatus.OK);
    }

    @GetMapping("/userinfo")
    public ResponseEntity<?> getUserInfo() {
        try {
            Optional<User> user = userService.findById(SecurityContext.getCurrentUser().getId());
            if (user.isPresent()) {
                User us = user.get();
                List<Role> roleByUser = userService.findRoleByUserIdAndActive(us, true, us.getClientId(), null);
                us.setRoles(roleByUser);
                us = userService.setAllOrgAndPositionUser(us);
                return new ResponseEntity<>(us, HttpStatus.OK);
            } else {
                return new ResponseEntity<>(HttpStatus.UNAUTHORIZED);
            }
        } catch (UnsupportedOperationException ex) {
            log.warn(" Exception get User infor", ex);
            return new ResponseEntity<>(HttpStatus.UNAUTHORIZED);
        }
    }

    @PostMapping(value = "/oauth/password")
    public ResponseEntity<?> changePassword(@RequestParam String oPassword, @RequestParam String nPassword) {
        User userInfo = SecurityContext.getCurrentUser();
        PasswordEncoder encoder = new BCryptPasswordEncoder();
        if (userInfo != null) {
            if (encoder.matches(oPassword, userInfo.getPassword())) {
                if (userInfo.isLdap()) {
                    LdapConnection ldapConnection = context.getBean(LdapConnection.class);
                    if (!ldapConnection.changePasswordLdap(userInfo.getUserName(), nPassword)) {
                        throw new RestExceptionHandler(Message.ACTION_FAILED);
                    }
                }
                userInfo.setPassword(encoder.encode(nPassword));
                userInfo.setChangePass(true);
                userInfo.setLastLogin(new Date());
                userInfo.setForgetPassword(false);
                return new ResponseEntity<>(userService.save(userInfo), HttpStatus.OK);
            }
            throw new RestExceptionHandler(Message.PASSWORD_INCORRECT);
        }
        throw new RestExceptionHandler(Message.INVALID_ACCOUNT);
    }

    @PostMapping(value = "/reset")
    public ResponseEntity<?> reset(@RequestParam List<Long> ids) {
        checkPermission();
        User u = BussinessCommon.getUser();

        PasswordEncoder encoder = new BCryptPasswordEncoder();
        String newPassword = encoder.encode(Constant.PASSWORD_DEFAULT);
        Long clientId = u.getClientId();
        if (ids.isEmpty()) {
            userService.resetAll(newPassword, clientId);
            return new ResponseEntity<>(new ResponseMessage("Reset mật khẩu tất cả tài khoản"), HttpStatus.OK);
        }
        if (!userService.isSameClientId(ids, u.getClientId())) {
            throw new RestExceptionHandler("Tài khoản để reset không hợp lệ");
        }

        userService.resetList(ids, newPassword, clientId);
        return new ResponseEntity<>(new ResponseMessage("Reset mật khẩu tất cả tài khoản được chọn"), HttpStatus.OK);
    }

    @PutMapping("/authen/{id}")
    public ResponseEntity<User> updateUser(@PathVariable long id) {
        User user = userService.findById(id).get();
        user.setActive(!user.getActive());
        return new ResponseEntity<>(userService.save(user), HttpStatus.OK);
    }

    @Override
    @PostMapping("/update/{id}")
    public ResponseEntity<?> update(@PathVariable Long id, @RequestBody User input) {
        boolean isAdmin = isAdmin(id);
        input.valids();

        User data = getService().findByClientIdAndId(getClientId(), id);
        if (data == null) {
            throw new RestExceptionHandler(Message.NOT_FOUND_USER);
        }

        // Check exist username
        if (!data.getUserName().equals(input.getUserName())) {
            User userInfo = userService.findByUserNameAndClientId(input.getUserName(), getClientId());
            if (userInfo != null) {
                throw new RestExceptionHandler(Message.EXIST_USER_NAME);
            }
            MapUserIAM mapUserIAM = mapUserIAMService.getMapUserByUserId();
            if (mapUserIAM != null && mapUserIAM.getCheckUser()) {
                throw new RestExceptionHandler(Message.USER_NAME_IAM_MODIFIED);
            } else {
                mapUserIAM.setCheckUser(true);
                mapUserIAMService.save(mapUserIAM);
                data.setUserName(input.getUserName());
            }
        }

        // Check exist email
        if (input.getEmail() != null && !input.getEmail().equals(data.getEmail())) {
            User tmpEmail = userService.getByEmail(input.getEmail());
            if (tmpEmail != null) {
                throw new RestExceptionHandler(Message.DUPLICATE_EMAIL);
            }
        }

        // Check exist serial token
        if (!StringUtils.isNullOrEmpty(input.getSerialToken())
                && !input.getSerialToken().equals(data.getSerialToken())) {
            User tmpSerialToken = userService.getBySerialToken(input.getSerialToken());
            if (tmpSerialToken != null) {
                userService.removeTokenExits(input.getSerialToken());
            }
        }
        if (!StringUtils.isNullOrEmpty(input.getCert()) && !input.getCert().equals(data.getCert())) {
            userService.removeCertExits(input.getCert());
        }

        boolean isWso2 = getClientId().compareTo(ldapClientId) == 0;
        if (isAdmin) {
            data.setUser(input, isWso2);
        } else {
            data.setUserLess(input, isWso2);
        }

        if (input.getAuthoritys() != null) {
            authoriryService.add(id, input.getAuthoritys(), null);
        }

        // Add secretary
        if (isAdmin) {
            secretaryService.save(id, input.getCecretarys());
        }

        try {
            userService.linkMultipleOrgAndPosition(input, id, true);
        } catch (Exception ex) {
            throw new RestExceptionHandler(ex.getMessage());
        }

        return new ResponseEntity<>(userService.save(data), HttpStatus.OK);
    }

    @PostMapping("/checkCert/{id}")
    public ResponseEntity<?> checkCert(@PathVariable Long id, @RequestBody User input) {
        User data = getService().findByClientIdAndId(getClientId(), id);
        if (data == null) {
            throw new RestExceptionHandler(Message.NOT_FOUND_USER);
        }
        if (!StringUtils.isNullOrEmpty(input.getCert())
                && !input.getCert().equals(data.getCert())) {
            return new ResponseEntity<>(userService.checkCertExits(input.getCert()), HttpStatus.OK);
        } else {
            Map<String, String> map = new HashMap<>();
            map.put("userName", null);
            return new ResponseEntity<>(map, HttpStatus.OK);
        }
    }

    @PostMapping("/checkToken/{id}")
    public ResponseEntity<?> checkToken(@PathVariable Long id, @RequestBody User input) {
        User data = getService().findByClientIdAndId(getClientId(), id);
        if (data == null) {
            throw new RestExceptionHandler(Message.NOT_FOUND_USER);
        }
        if (!StringUtils.isNullOrEmpty(input.getSerialToken())
                && !input.getSerialToken().equals(data.getSerialToken())) {
            return new ResponseEntity<>(userService.checkTokenExits(input.getSerialToken()), HttpStatus.OK);
        } else {
            Map<String, String> map = new HashMap<>();
            map.put("userName", null);
            return new ResponseEntity<>(map, HttpStatus.OK);
        }
    }

    @PostMapping("/updateDefaultRole")
    public ResponseEntity<?> updateDefaultRole(@RequestParam Long roleId) {
        List<Long> roleByUser = userService.findRoleIdByUserIdAndActive(BussinessCommon.getUser(), true,
                BussinessCommon.getClientId());
        if (!roleByUser.contains(roleId)) {
            return new ResponseEntity<>(false, HttpStatus.UNAUTHORIZED);
        }
        User data = getService().findByClientIdAndId(getClientId(), getCurrentUser().getId());
        if (data != null) {
            data.setDefaultRole(roleId);
        }
        getService().save(data);
        return new ResponseEntity<>(true, HttpStatus.OK);
    }

    @PostMapping(value = "/findUser")
    public ResponseEntity<Page<User>> findUser(@RequestParam(value = "fullName", required = false) String fullName,
                                               @RequestParam(value = "userName", required = false) String userName,
                                               @RequestParam(value = "email", required = false) String email, @RequestParam(value = "phone", required = false) String phone,
                                               @RequestParam(value = "sex", required = false) Long sex,
                                               @RequestParam(value = "indentity", required = false) String indentity,
                                               @RequestParam(value = "title", required = false) String title,
                                               @RequestParam(value = "nameToken", required = false) String nameToken,
                                               @RequestParam(value = "serialToken", required = false) String serialToken,
                                               @RequestParam(value = "employeeId", required = false) String employeeId,
                                               @RequestParam(value = "employeeCode", required = false) String employeeCode,
                                               @RequestParam(value = "salt", required = false) String salt,
                                               @RequestParam(value = "org", required = false) String org,
                                               @RequestParam(value = "position", required = false) String position,
                                               @RequestParam(value = "lead", required = false) Boolean lead,
                                               @RequestParam(value = "birthday", required = false) @DateTimeFormat(iso = ISO.DATE) Date birthday,
                                               @RequestParam(defaultValue = "ACTIVE", required = false) SortBy sortBy,
                                               @RequestParam(defaultValue = "ASC", required = false) Direction direction,
                                               @RequestParam(defaultValue = "" + Constant.NUMBER_OF_PAGE, required = false) int size,
                                               @RequestParam(defaultValue = "1", required = false) int page) {
        fullName = StringUtils.handleSubmit(fullName);
        userName = StringUtils.handleSubmit(userName);
        email = StringUtils.handleSubmit(email);
        phone = StringUtils.handleSubmit(phone);
        indentity = StringUtils.handleSubmit(indentity);
        title = StringUtils.handleSubmit(title);
        nameToken = StringUtils.handleSubmit(nameToken);
        serialToken = StringUtils.handleSubmit(serialToken);
        employeeCode = StringUtils.handleSubmit(employeeCode);
        salt = StringUtils.handleSubmit(salt);
        Long employeeIdT = employeeId != null && employeeId.length() > 0 ? Long.parseLong(employeeId) : null;
        Long orgT = org != null && org.length() > 0 ? Long.parseLong(org) : null;
        Long positionT = position != null && position.length() > 0 ? Long.parseLong(position) : null;
        Date birthdayT = birthday != null && birthday.toString().length() > 0 ? birthday : null;
        Sort sort = Sort.by(direction, sortBy.field);
        Pageable pageable = PageRequest.of(page - 1, size, sort);
        return new ResponseEntity<>(userService.findUser(fullName, userName, email, phone, sex, indentity, title,
                nameToken, serialToken, employeeIdT, employeeCode, salt, orgT, positionT, lead, birthdayT, pageable),
                HttpStatus.OK);
    }

    @GetMapping("/findListUserByPosition/{positionId}")
    public ResponseEntity<?> findListUserByPosition(@PathVariable Long positionId) {
        catService.validPositionId(positionId);
        return new ResponseEntity<>(userService.findListUserByPosition(positionId), HttpStatus.OK);
    }

    @GetMapping("/findListUserByOrg/{orgId}")
    public ResponseEntity<?> findListUserByOrg(@PathVariable Long orgId) {
        orgService.validOrgId(orgId);
        return new ResponseEntity<>(userService.findByOrgIdAndClientId(orgId, BussinessCommon.getClientId()),
                HttpStatus.OK);
    }

    @GetMapping("/getListNguoiUyQuyenVanThuBan")
    public ResponseEntity<List<User>> getListNguoiUyQuyenVanThuBan() {
        return new ResponseEntity<>(userService.getListNguoiUyQuyenVanThuBan(), HttpStatus.OK);
    }

    @GetMapping("/getListNguoiUyQuyen")
    public ResponseEntity<?> getListNguoiUyQuyen() {
        return new ResponseEntity<>(userService.getListNguoiUyQuyen(), HttpStatus.OK);
    }

    @GetMapping("/findListDelegateUser/{userId}")
    public ResponseEntity<?> findListDelegateUser(@PathVariable Long userId) {
        return new ResponseEntity<>(userService.findListNguoiDuocUyQuyen(userId), HttpStatus.OK);
    }

    @PostMapping("/all-user-in-org")
    public ResponseEntity<?> allUserInOrg(@RequestParam("textSearch") String textSearch, @RequestParam("orgId") Long orgId) {
        if (textSearch == null || textSearch.length() == 0 || textSearch.equals("undefined")) {
            textSearch = "";
        }
        textSearch = textSearch.toLowerCase();
        return new ResponseEntity<>(userService.allUserInOrg(orgId, textSearch, true), HttpStatus.OK);
    }

    @GetMapping("/all-user-in-org-paging")
    public ResponseEntity<?> allUserInOrgPaging(@RequestParam("textSearch") String textSearch,
                                                @RequestParam(value = "page", defaultValue = "1") Integer page,
                                                @RequestParam(value = "active", required = false) Boolean active) {
        Long orgId = BussinessCommon.getUser().getOrg();
        if (textSearch == null || textSearch.length() == 0 || textSearch.equals("undefined")) {
            textSearch = "";
        }
        textSearch = textSearch.toLowerCase();
        Pageable pageable = BussinessCommon.castToPageable(page);
        return new ResponseEntity<>(userService.allUserInOrg(orgId, textSearch, active, pageable), HttpStatus.OK);
    }

    @PostMapping("/all-user-in-org-type")
    public ResponseEntity<List<User>> allUserInOrgType(@RequestParam("textSearch") String textSearch) {
        User user = BussinessCommon.getUser();
        Long org = user.getOrg();
        if (!orgService.isUserOfOrgType(user, Constant.BAN)
                && !orgService.isUserOfOrgType(user, Constant.CUC_VU_VIEN)) {
            org = orgService.getParentByOrgType(user.getOrgModel(), Constant.CUC_VU_VIEN);

            if (org == null) {
                org = orgService.getParentByOrgType(user.getOrgModel(), Constant.BAN);
            }
        }

        if (textSearch == null || textSearch.length() == 0 || textSearch.equals("undefined")) {
            textSearch = "";
        }
        textSearch = textSearch.toLowerCase();
        return new ResponseEntity<>(userService.allUserInOrg(org, textSearch, true), HttpStatus.OK);
    }

    @PostMapping("/all-user-in-org-type-paging")
    public ResponseEntity<?> allUserInOrgTypePaging(@RequestParam("textSearch") String textSearch,
                                                    @RequestParam(defaultValue = "1") Integer page) {
        User user = BussinessCommon.getUser();
        Long org = user.getOrg();
        if (!orgService.isUserOfOrgType(user, Constant.BAN)
                && !orgService.isUserOfOrgType(user, Constant.CUC_VU_VIEN)) {
            org = orgService.getParentByOrgType(user.getOrgModel(), Constant.CUC_VU_VIEN);

            if (org == null) {
                org = orgService.getParentByOrgType(user.getOrgModel(), Constant.BAN);
            }
        }

        if (textSearch == null || textSearch.length() == 0 || textSearch.equals("undefined")) {
            textSearch = "";
        }
        textSearch = textSearch.toLowerCase();
        Pageable pageable = BussinessCommon.castToPageable(page);
        return new ResponseEntity<>(userService.allUserInOrg(org, textSearch, true, pageable), HttpStatus.OK);
    }

    @Override
    @GetMapping("/getAllSort/{direction}/{column}")
    public ResponseEntity<?> getByClientIdAndSort(@PathVariable String direction,
                                                  @RequestParam(required = false, defaultValue = "true") Boolean active, @PathVariable String column) {
        return new ResponseEntity<>(userService.getByClientIdAndActiveAndSort(direction, active, column),
                HttpStatus.OK);
    }

    @GetMapping("/getAllOrder")
    public ResponseEntity<List<UserTreeDto>> getAllOrder() {
        return new ResponseEntity<>(userService.getAllOrder(), HttpStatus.OK);
    }

    @GetMapping("/getByAuthority")
    public ResponseEntity<List<UserTreeDto>> getByAuthority() {
        return new ResponseEntity<>(authoriryService.getByAuthority(AuthorityEnum.LEADERSHIP), HttpStatus.OK);
    }

    @GetMapping("/getListLDCuc")
    public ResponseEntity<List<UserInfoDto>> getListLDCuc() {
        return new ResponseEntity<>(authoriryService.getListLDCuc(), HttpStatus.OK);
    }

    @GetMapping(value = "/getAllUser")
    public ResponseEntity<?> getAllUser(@PathParam(value = "page") Integer page) {
        return new ResponseEntity<>(userService.getAllUser(page), HttpStatus.OK);
    }

    @GetMapping(value = "/getAllUserByLead")
    public ResponseEntity<?> getAllUserByLead() {
        return new ResponseEntity<>(userService.getAllUserByLead(), HttpStatus.OK);
    }

    @GetMapping(value = "/get_all_user")
    public ResponseEntity<?> getAllUserDtoByActive() {
        return new ResponseEntity<>(userService.getAllUserDtoByActive(), HttpStatus.OK);
    }

    @PostMapping(value = "/findByUserName")
    public ResponseEntity<?> findByUserName(@PathParam(value = "userName") String userName) {
        return new ResponseEntity<>(userService.findByUserName(userName), HttpStatus.OK);
    }

    @PostMapping(value = "/findByUserId")
    public ResponseEntity<?> findByUserId(@PathParam(value = "userId") Long userId) {
        return new ResponseEntity<>(userService.findByUserId(userId), HttpStatus.OK);
    }

    @GetMapping("/switchRole/{roleId}")
    public ResponseEntity<?> switchRole(@PathVariable Long roleId) {
        List<Long> roleByUser = userService.findRoleIdByUserIdAndActive(BussinessCommon.getUser(), true,
                BussinessCommon.getClientId());
        if (roleByUser.contains(roleId)) {
            List<Module> moduleOfUser = moduleService.findByRoleIdList(Arrays.asList(roleId), null);
            userService.setCurrentRole(BussinessCommon.getUserId(), roleId);
            return new ResponseEntity<>(moduleOfUser, HttpStatus.OK);
        }
        return new ResponseEntity<>(new ArrayList<>(), HttpStatus.UNAUTHORIZED);
    }

    @Override
    @GetMapping("/active/{id}")
    public ResponseEntity<?> active(@PathVariable Long id) {
        checkPermission();
        User data = getService().findByClientIdAndId(getClientId(), id);
        if (data == null || data.getActive() != null && data.getActive()) {
            log.error("Data object is null");
            return new ResponseEntity<>(HttpStatus.NOT_FOUND);
        }

        if (!orgService.existsByIdAndActive(data.getOrg(), true)) {
            throw new RestExceptionHandler("Đơn vị không tồn tại hoặc không còn hoạt động");
        }
        data.setActive(true);
        data = getService().save(data);
        return new ResponseEntity<>(data, HttpStatus.OK);
    }

    @Override
    @GetMapping("/deactive/{id}")
    public ResponseEntity<?> deactive(@PathVariable Long id) {
        checkPermission();
        User data = getService().findByClientIdAndId(getClientId(), id);
        if (data == null || !data.getActive()) {
            log.error("Data object is null");
            return new ResponseEntity<>(HttpStatus.NOT_FOUND);
        }
        data.setActive(false);
        data = getService().save(data);
        return new ResponseEntity<>(data, HttpStatus.OK);
    }

    @PostMapping(value = "/getProfile")
    public ResponseEntity<?> getProfile(@RequestParam(value = "userName") String userName) {
        if (!BussinessCommon.getUser().getUserName().equals(userName)) {
            throw new RestExceptionHandler("Bạn không có quyền truy cập vào người dùng");
        }
        return new ResponseEntity<>(userService.findByUserName(userName), HttpStatus.OK);
    }

    @GetMapping(value = "/isClerical")
    public ResponseEntity<?> isClerical(@RequestParam DocumentTypeEnum docType) {
        return new ResponseEntity<>(userService.isClerical(docType), HttpStatus.OK);
    }

    @GetMapping("/findUserByOrgAndPositionAndFullName")
    public ResponseEntity<?> findUserByOrgAndPositionAndFullName(@RequestParam(required = false) Long orgId,
                                                                 @RequestParam(required = false) Long posId, @RequestParam(required = false) String fullName,
                                                                 @RequestParam(defaultValue = "1") Integer page) {
        return new ResponseEntity<>(userService.findUserByOrgAndPositionAndFullName(orgId, posId, fullName, page), HttpStatus.OK);
    }

    @GetMapping("/findUserByOrgWithAuthority")
    public ResponseEntity<?> findUserByOrgWithAuthority(@RequestParam Long orgId, @RequestParam AuthorityEnum authority) {
        return new ResponseEntity<>(userService.findUserByOrgWithAuthority(orgId, authority), HttpStatus.OK);
    }

    @GetMapping("/findLeadership")
    public ResponseEntity<?> findLeadership(@RequestParam Long orgId) {
        return new ResponseEntity<>(userService.findLeadershipByOrgId(orgId), HttpStatus.OK);
    }

    @GetMapping("/findLanhDaoKy")
    public ResponseEntity<?> findLanhDaoKy() {
        return new ResponseEntity<>(userService.findLanhDaoKy(), HttpStatus.OK);
    }

    @GetMapping("/findUserByOrgAndChildWithAuthority")
    public ResponseEntity<?> findUserByOrgAndChildWithAuthority(@RequestParam Long orgId, @RequestParam AuthorityEnum authority) {
        return new ResponseEntity<>(userService.findUserByOrgAndChildWithoutAuthority(orgId), HttpStatus.OK);
    }

    @GetMapping("/all-user-in-org")
    public ResponseEntity<?> allUserInOrgs(@RequestParam("textSearch") String textSearch) {
        Long orgId = BussinessCommon.getUser().getOrg();
        if (textSearch == null || textSearch.length() == 0 || textSearch.equals("undefined")) {
            textSearch = "";
        }
        textSearch = textSearch.toLowerCase();
        return new ResponseEntity<>(userService.allUserInOrgs(orgId, textSearch, true), HttpStatus.OK);
    }

    /**
     * for document-out ask
     *
     * @return
     */
    @GetMapping("/tree")
    public ResponseEntity<?> getTree() {
        return new ResponseEntity<>(userService.getTree(), HttpStatus.OK);
    }

    /**
     * Get user id by fullName
     *
     * @return
     */
    @GetMapping("/getUserIdByFullNames")
    public ResponseEntity<?> getUserIdByFullName(@RequestParam String[] fullNames) {
        return new ResponseEntity<>(userService.findUserIdByFullName(fullNames), HttpStatus.OK);
    }

    @GetMapping("/forget-password")
    public ResponseEntity<?> forgetPassword(@RequestParam String email) {
        return new ResponseEntity<>(userService.forgetPasword(email), HttpStatus.OK);
    }

    /**
     * Remember me handle
     *
     * @return
     */
    @GetMapping("/refresh-token")
    public ResponseEntity<?> refreshToken(@RequestParam(required = false, defaultValue = "TRUE") Boolean rememberPassword) {
        User request = BussinessCommon.getUser();
        User userInfo = userService.findByUserNameAndActive(request.getUserName(), true);
        if (userInfo == null || !Boolean.TRUE.equals(userInfo.getRememberPassword())
                || isExpiredToken(userInfo.getLastLogin())) {
            return new ResponseEntity<>(new AuthenResponse(), HttpStatus.OK);
        }

        Token tokenInfo = tokenProvider.generateToken(userInfo.getUserName());
        List<Module> moduleAll = getAllModule(userInfo);
        userInfo.setAuthoritys(authoriryService.get(userInfo.getId(), userInfo.getClientId(), true));
        userInfo.setCecretarys(secretaryService.findSecretaryByBossId(userInfo.getId(), userInfo.getClientId(), true));
        AuthenResponse response = new AuthenResponse(tokenInfo, userInfo, moduleAll);

        if (Boolean.FALSE.equals(rememberPassword)) {
            userInfo.setRememberPassword(false);
        }

        userInfo.setLastLogin(new Date());
        userService.save(userInfo);
        straceService.save(userInfo.getId(), ActionEnum.LOGIN.getName(), userInfo.getUserName(),
                CategoryEnum.LOGIN.getValue(), userInfo);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    /**
     * Is expired token
     *
     * @param lastLogin
     * @return
     */
    private boolean isExpiredToken(Date lastLogin) {
        if (lastLogin == null)
            return false;
        long currentTime = new Date().getTime();
        long usedTime = currentTime - lastLogin.getTime();
        return usedTime >= this.timeExpireRefreshToken;
    }

    @PostMapping("/getListUserCerByGroupAndOrg")
    public ResponseEntity<?> getListUserCerByGroupAndOrg(@RequestBody RequestCerOrgAndGroup dto) {
        return new ResponseEntity<>(userService.getListUserIdByOrgAndGroup(dto), HttpStatus.OK);
    }

    @PostMapping("/findUserByOrgAndPositionUsingGroupUser")
    public ResponseEntity<?> findUserByOrgAndPositionUsingGroupUser(@RequestBody GroupWithListUserDto orgId,
                                                                    @RequestParam(required = false) Long posId, @RequestParam(required = false) String fullName,
                                                                    @RequestParam(defaultValue = "1") Integer page) {
        List<Long> orgIds = new ArrayList<>();
        if (orgId != null) {
            orgIds = orgId.getListUser();
        }
        return new ResponseEntity<>(userService.findUserByOrgAndPositionUsingGroupUser(orgIds, posId, fullName, page), HttpStatus.OK);
    }

    @PostMapping("/loginIAM")
    public ResponseEntity<?> authentication(@RequestParam String userName, @RequestParam String name, @RequestParam String clientIAM) {
        if (StringUtils.isNullOrEmpty(userName) || StringUtils.isNullOrEmpty(name) || StringUtils.isNullOrEmpty(clientIAM)) {
            throw new RestExceptionHandler(Message.NO_INPUT_DATA);
        }
        if (!clientIAM.equals(Constant.CLIENT_IAM)) {
            throw new RestExceptionHandler(Message.CLIENT_IAM_NOT_FOUND);
        }
        PasswordEncoder encoder = new BCryptPasswordEncoder();
        MapUserIAM mapUserIAM = mapUserIAMService.getMapUserBySubId(userName);
        User userInfo = new User();
        if (mapUserIAM == null) {
            Category category = catService.findByClientIdAndNameAndCode(Constant.CAT_POSITION, Constant.ORG_CAT_IAM);
            Organization organization = orgService.findByIdentifierAndClientIdAndActiveTrue(Constant.ORG_CODE_IAM);
            userInfo = userService.findByUserNameAndActive(userName, true);
            User userInfoNew = new User();
            if (userInfo == null) {
                userInfoNew.setPassword(encoder.encode(Constant.PASSWORD_DEFAULT));
                userInfoNew.setUserName(userName);
                userInfoNew.setFullName(name);
                userInfoNew.setOrg(organization.getId());
                userInfoNew.setPosition(category.getId());
                userInfoNew.setClientId(organization.getClientId());
                userService.save(userInfoNew);
                userInfo = userInfoNew;
            }
            MapUserIAM mapUserIAMNew = new MapUserIAM();
            mapUserIAMNew.setUserId(userInfo.getId());
            mapUserIAMNew.setSubId(userName);
            mapUserIAMNew.setClientId(userInfo.getClientId());
            mapUserIAMNew.setCreateBy(userInfo.getId());
            mapUserIAMNew.setCheckUser(false);
            mapUserIAMService.save(mapUserIAMNew);
        } else {
            userInfo = mapUserIAM.getUser();
            if (userInfo.getPassword() == null) {
                userInfo.setPassword(encoder.encode(Constant.PASSWORD_DEFAULT));
                userService.save(userInfo);
            }
        }
        checkExpireClient(userInfo);
        Token tokenInfo = tokenProvider.generateToken(userInfo.getUserName());
        List<Module> moduleAll = getAllModule(userInfo);
        userInfo.setAuthoritys(authoriryService.get(userInfo.getId(), userInfo.getClientId(), true));
        userInfo.setCecretarys(secretaryService.findSecretaryByBossId(userInfo.getId(), userInfo.getClientId(), true));
        userInfo = userService.setAllOrgAndPositionUser(userInfo);
        AuthenResponse response = new AuthenResponse(tokenInfo, userInfo, moduleAll);
        userInfo.setLastLogin(new Date());
        userService.save(userInfo);
        straceService.save(userInfo.getId(), ActionEnum.LOGIN.getName(), userName, CategoryEnum.LOGIN.getValue(),
                userInfo);
        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @PostMapping("/mapUserIAM")
    public ResponseEntity<?> mapUserIAM(@RequestParam String userName, @RequestParam String clientIAM, @RequestParam Long userId) {
        if (StringUtils.isNullOrEmpty(userName) || StringUtils.isNullOrEmpty(clientIAM) || userId == null) {
            throw new RestExceptionHandler(Message.NO_INPUT_DATA);
        }
        if (!clientIAM.equals(Constant.CLIENT_IAM)) {
            throw new RestExceptionHandler(Message.CLIENT_IAM_NOT_FOUND);
        }
        MapUserIAM mapUserIAM = mapUserIAMService.getMapUserBySubId(userName);
        User userInfo = new User();
        if (mapUserIAM == null) {
            MapUserIAM mapUserIAMNew = new MapUserIAM();
            mapUserIAMNew.setUserId(userId);
            mapUserIAMNew.setSubId(userName);
            mapUserIAMNew.setCheckUser(true);
            mapUserIAMService.save(mapUserIAMNew);
            userInfo = mapUserIAMNew.getUser();
        } else {
            if (!userId.equals(mapUserIAM.getUserId()) && mapUserIAM.getCheckUser()) {
                throw new RestExceptionHandler(Message.USER_LINK_IAM);
            }
            if (!userId.equals(mapUserIAM.getUserId()) && !mapUserIAM.getCheckUser()) {
                User user = userService.findByUserId(mapUserIAM.getUserId());
                user.setActive(false);
                userService.save(user);
                mapUserIAM.setUserId(userId);
                mapUserIAMService.save(mapUserIAM);
                userInfo = mapUserIAM.getUser();
            }
        }
        return new ResponseEntity<>(userInfo, HttpStatus.OK);
    }

    @GetMapping("/check-map-iam")
    public ResponseEntity<?> checkMapIAM() {
        return new ResponseEntity<>(mapUserIAMService.getMapUserByUserId(), HttpStatus.OK);
    }

    @PostMapping("/checkTokenOfUser")
    public ResponseEntity<?> checkTokenOfUser(@RequestBody String token) {
        return new ResponseEntity<>(userService.checkTokenOfUser(token), HttpStatus.OK);
    }

    @PostMapping("/additionalPosition/add/{userId}")
    public ResponseEntity<?> assignAdditionalPosition(@PathVariable("userId") long userId, @RequestBody Map<String, ?> body) {
        List<Integer> positions = (List<Integer>) body.get("positions");
        return new ResponseEntity<>(userService.addAdditionalPositions(userId, positions), HttpStatus.OK);
    }

    @PostMapping("/additionalPosition/remove/{userId}")
    public ResponseEntity<?> removeAdditionalPosition(@PathVariable("userId") long userId, @RequestBody Map<String, ?> body) {
        List<Integer> positions = (List<Integer>) body.get("positions");
        return new ResponseEntity<>(userService.removeAdditionalPositions(userId, positions), HttpStatus.OK);
    }

    @GetMapping("/getListUserByOrgIdAndPosId")
    public ResponseEntity<?> findListUserByPositionId(@PathParam("orgId") long orgId, @PathParam("posId") long posId) {
        return new ResponseEntity<>(userService.findListUserByOrgAndPositionId(orgId, posId), HttpStatus.OK);
    }

    @GetMapping("/switchOrgPosition/{orgId}/{positionId}")
    public ResponseEntity<?> switchOrgPosition(@PathVariable Long orgId, @PathVariable Long positionId) {
        orgService.validOrgId(orgId);
        catService.validCatId(positionId);
        User user = BussinessCommon.getUser();
        user.setOrg(orgId);
        user.setPosition(positionId);
        User us = userRepository.save(user);
        return new ResponseEntity<>(us, HttpStatus.OK);
    }
}
