package com.vz.backend.core.controller;

import java.io.IOException;
import java.util.Base64;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.CategoryInitDto;
import com.vz.backend.core.dto.IdName;
import com.vz.backend.core.dto.OrgDto;
import com.vz.backend.core.dto.OrgGroupDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.exception.RestForbidden;
import com.vz.backend.core.service.IService;
import com.vz.backend.core.service.OrganizationService;
import com.vz.backend.core.service.RoleService;

import lombok.extern.slf4j.Slf4j;
import org.springframework.web.multipart.MultipartFile;

/**
 * @author DucND
 * @date Apr 15, 2020
 */
@Slf4j
@RestController
@RequestMapping("/org")
public class OrganizationController {

    @Autowired
    private OrganizationService orgService;

    public IService<Organization> getService() {
        return orgService;
    }

    @Autowired
    private RoleService roleService;

    private void checkPermission() {
        if (roleService.existUserInModule(ModuleCodeEnum.ORG.getName())) {
            return;
        }
        throw new RestForbidden("Bạn không có quyền truy cập vào người dùng");
    }

    @PostMapping(path = "/search/{page}")
    public ResponseEntity<?> getList(@RequestBody OrgDto dto, @PathVariable Integer page) {
        return new ResponseEntity<>(orgService.search(dto, page), HttpStatus.OK);
    }

    @GetMapping(path = "/findByNoParent")
    public ResponseEntity<?> findByNoParent(@RequestParam(required = false) Boolean active) {
        return new ResponseEntity<>(orgService.findByNoParent(active), HttpStatus.OK);
    }

    @GetMapping(path = "/findByOrgCVV")
    public ResponseEntity<?> findByParentIdAndActiveAAndClientId() {
        return new ResponseEntity<>(orgService.findByParentIdAndActiveAAndClientId(), HttpStatus.OK);
    }

    @GetMapping(path = "/root2")
    public ResponseEntity<List<Long>> root2(@RequestParam(value = "q", required = false) Long orgId) {
        User user = BussinessCommon.getUser();
        if (orgId == null) {
            orgId = user.getOrg();
        }
        return new ResponseEntity<>(orgService.listParentOrg(orgId), HttpStatus.OK);
    }

    @GetMapping(path = "/root")
    public ResponseEntity<Long> root(@RequestParam(value = "q", required = false) Long orgId) {
        User user = BussinessCommon.getUser();
        if (orgId == null) {
            orgId = user.getOrg();
        }
        return new ResponseEntity<>(orgService.getRootOrgId(orgId), HttpStatus.OK);
    }

    @GetMapping(path = "/find_all_org")
    public ResponseEntity<?> findAllOrg() {
        return new ResponseEntity<>(orgService.getListDtoByClientId(), HttpStatus.OK);
    }

    @GetMapping("/deactive/{id}")
    public ResponseEntity<Organization> deactive(@PathVariable Long id) {
        checkPermission();
        if (orgService.existUserByOrgId(id)) {
            log.error("Xóa đơn vị không thành công: Đang có người dùng thuộc đơn vị này!!!");
            throw new RestExceptionHandler("Xóa đơn vị không thành công: Đang có người dùng thuộc đơn vị này");
        }
        if (orgService.existChildByOrgId(id)) {
            throw new RestExceptionHandler("Xóa đơn vị không thành công: Đang có đơn vị con thuộc đơn vị này");
        }
        Organization data = getService().findByClientIdAndId(BussinessCommon.getClientId(), id);
        if (data == null || !data.getActive()) {
            log.error("Data object is null");
            return new ResponseEntity<>(HttpStatus.NOT_FOUND);
        } else {
            data.setActive(false);
            getService().save(data);
            return new ResponseEntity<>(HttpStatus.OK);
        }
    }

    @GetMapping("/active/{id}")
    public ResponseEntity<?> active(@PathVariable Long id) {
        checkPermission();
        if (orgService.hasInActiveParent(id)) {
            log.error("Đơn vị cha chưa đc active");
            throw new RestExceptionHandler("Đơn vị cha chưa được active");
        }
        Organization data = getService().findByClientIdAndId(BussinessCommon.getClientId(), id);
        if (data == null || data.getActive() != null && data.getActive()) {
            log.error("Data object is null");
            return new ResponseEntity<>(null, HttpStatus.NOT_FOUND);
        } else {
            data.setActive(true);
            data = getService().save(data);
            return new ResponseEntity<>(data, HttpStatus.OK);
        }
    }

    @PostMapping("/add")
    public ResponseEntity<?> create(@RequestBody Organization input) {
        checkPermission();
        return new ResponseEntity<>(orgService.addOrg(input), HttpStatus.OK);
    }

    @PostMapping(value="/update/{id}", consumes = {MediaType.MULTIPART_FORM_DATA_VALUE})
    public ResponseEntity<?> update(@PathVariable Long id, @RequestPart("organization") Organization input,
                                    @RequestPart(value = "logo", required = false) MultipartFile file) throws IOException {
        checkPermission();
        if(file != null){
            byte[] fileContent = file.getBytes();
            String encodedString = Base64.getEncoder().encodeToString(fileContent);
            input.setLogo("data:" + file.getContentType() + ";base64," + encodedString);
        }
        return ResponseEntity.ok(orgService.updateOrg(id, input));
    }

    @GetMapping("/getAllSort/{direction}/{column}")
    public ResponseEntity<?> getByClientIdAndSort(@PathVariable String direction,
                                                  @RequestParam(required = false) Boolean active, @PathVariable String column) {
        Sort sort;
        if (direction.equals("ASC")) {
            sort = Sort.by(Direction.ASC, column);
        } else {
            sort = Sort.by(Direction.DESC, column);
        }
        List<Organization> data = getService().findByClientIdAndActive(BussinessCommon.getClientId(), active, sort);
        return new ResponseEntity<>(data, HttpStatus.OK);
    }

    @GetMapping(path = "/findCuc/{orgId}")
    public ResponseEntity<?> findCuc(@PathVariable Long orgId) {
        Organization org = orgService.findByClientIdAndId(BussinessCommon.getClientId(), orgId);
        return new ResponseEntity<>(orgService.getOrgAndSubByOrgType(org, Constant.CUC_VU_VIEN), HttpStatus.OK);
    }

    @GetMapping(path = "/find_all_org_sub")
    public ResponseEntity<List<CategoryInitDto>> findAllOrgSub(
            @RequestParam(required = false, defaultValue = "FALSE") Boolean showAll) {
        User user = BussinessCommon.getUser();
        Long org = user.getOrg();
        if (Boolean.TRUE.equals(showAll)) {
            org = orgService.getRootOrgId(org);
        } else {
            if (!orgService.isUserOfOrgType(user, Constant.BAN)
                    && !orgService.isUserOfOrgType(user, Constant.CUC_VU_VIEN)) {
                org = orgService.getParentByOrgType(user.getOrgModel(), Constant.CUC_VU_VIEN);

                if (org == null) {
                    org = orgService.getParentByOrgType(user.getOrgModel(), Constant.BAN);
                }
            }
        }

        List<Long> orgSub = orgService.orgAndSub(org);

        return new ResponseEntity<>(orgService.getOrgAndSub(orgSub), HttpStatus.OK);
    }

    @GetMapping("/getOrgTreeById")
    public ResponseEntity<?> getOrgTreeById(@RequestParam Long id) {
        return new ResponseEntity<>(orgService.getOrgTreeById(id), HttpStatus.OK);
    }

    @GetMapping(path = "/findParentByOrgId/{orgId}")
    public ResponseEntity<?> findParentByOrgId(@PathVariable Long orgId) {
        return new ResponseEntity<>(orgService.findParentByOrgId(orgId), HttpStatus.OK);
    }

    @GetMapping(path = "/findAllOrgAndSub/{orgId}")
    public ResponseEntity<?> findAllOrgAndSub(@PathVariable Long orgId) {
        return new ResponseEntity<>(orgService.orgAndSubWithName(orgId), HttpStatus.OK);
    }

    @GetMapping(path = "/findAllByParentId")
    public ResponseEntity<?> findAllOrgAndSub2(@RequestParam(required = false) Long parentId) {
        return new ResponseEntity<>(orgService.orgAndSubWithName(parentId), HttpStatus.OK);
    }

    @GetMapping(path = "/get/{id}")
    public ResponseEntity<?> get(@PathVariable Long id) {
        return new ResponseEntity<>(orgService.findByClientIdAndId(BussinessCommon.getClientId(), id), HttpStatus.OK);
    }

    @GetMapping(path = "/findAllSiblings")
    public ResponseEntity<?> findAllOrgAndSub() {
        return new ResponseEntity<>(orgService.getAllSiblings(), HttpStatus.OK);
    }

    @GetMapping(value = "/search-org")
    public ResponseEntity<List<OrgGroupDto>> searchOrg(@RequestParam("q") String name,
                                                       @RequestParam(required = false, defaultValue = "FALSE") Boolean showAll) {
        User user = BussinessCommon.getUser();
        Long org = user.getOrg();
        if (Boolean.TRUE.equals(showAll)) {
            org = orgService.getRootOrgId(org);
        } else {
            if (!orgService.isUserOfOrgType(user, Constant.BAN)
                    && !orgService.isUserOfOrgType(user, Constant.CUC_VU_VIEN)) {
                org = orgService.getParentByOrgType(user.getOrgModel(), Constant.CUC_VU_VIEN);
                if (org == null) {
                    org = orgService.getParentByOrgType(user.getOrgModel(), Constant.BAN);
                }
            }
        }
        List<Long> orgSub = orgService.orgAndSubByName(org, name.toLowerCase());
        return new ResponseEntity<>(orgService.getOrgGroupDtoAndSub(orgSub), HttpStatus.OK);
    }

	@GetMapping("/get-all/global")
	public ResponseEntity<List<IdName>> getAllByGlobal() {
		return new ResponseEntity<>(orgService.getAllByGlobal(), HttpStatus.OK);
	}

    @GetMapping(path = "/getTreeOrg")
    public ResponseEntity<?> getTreeOrg(@RequestParam("type") Long type) {
        return new ResponseEntity<>(orgService.getTreeOrg(type), HttpStatus.OK);
    }

    @GetMapping(path = "/getAllById")
    public ResponseEntity<?> getAllById(@RequestParam("orgId") Long orgId) {
        return new ResponseEntity<>(orgService.getAllById(orgId), HttpStatus.OK);
    }

    @GetMapping(path = "/getAllByText")
    public ResponseEntity<?> getAllByText(@RequestParam("textSearch") String text) {
        return new ResponseEntity<>(orgService.getAllByText(text), HttpStatus.OK);
    }
}
