package com.vz.backend.core.service;

import java.util.*;
import java.util.stream.Collectors;

import com.vz.backend.core.config.Constant;
import com.vz.backend.core.dto.*;
import com.vz.backend.core.repository.IUserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.OrgConfigSign;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.exception.RestFieldExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.repository.OrganizationRepository;
import com.vz.backend.util.StringUtils;
import org.springframework.transaction.annotation.Transactional;

/**
 * @author DucND
 * @date Apr 14, 2020
 */
@Service
public class OrganizationService extends BaseService<Organization> {

    @Autowired
    private OrganizationRepository orgRepository;

    @Autowired
    private IUserRepository userRepo;

    @Override
    public IRepository<Organization> getRepository() {
        return orgRepository;
    }

    public Organization findByClientIdAndId(Long orgId) {
        return orgRepository.findByClientIdAndId(BussinessCommon.getClientId(), orgId);
    }

    public Organization findByClientIdAndId(Long clientId, Long orgId) {
        return orgRepository.findByClientIdAndId(clientId, orgId);
    }

    public Organization validOrgId(Long orgId) {
        Organization org = findByClientIdAndId(orgId);
        if (org == null) {
            throw new RestExceptionHandler(Message.NOT_FOUND_ORG);
        }
        return org;
    }

    public Organization validOrgId(Long clientId, Long orgId) {
        Organization org = findByClientIdAndId(clientId, orgId);
        if (org == null) {
            throw new RestExceptionHandler(Message.NOT_FOUND_ORG);
        }
        return org;
    }

    public List<Organization> findByNoParent(Boolean active) {
        Sort sort = Sort.by(Direction.ASC, "id");
        return orgRepository.findByClientIdAndParentIdAndActive(BussinessCommon.getClientId(), null, active, sort);
    }

    public List<Organization> findByClientIdAndParentIdAndActive(Long orgId, Boolean active) {
        Sort sort = Sort.by(Direction.ASC, "id");
        return orgRepository.findByClientIdAndParentIdAndActive(BussinessCommon.getClientId(), orgId, active, sort);
    }

    public Long findParentIdByOrgId(Long orgId) {
        return orgRepository.findParentIdByOrgIdAndClientId(orgId, BussinessCommon.getClientId());
    }

    public OrganizationBasicDto findParentByOrgId(Long orgId) {
        OrganizationBasicDto org = orgRepository.findParentByOrgIdAndClientId(orgId, BussinessCommon.getClientId());
        if (org == null) {
            org = orgRepository.findByOrgIdAndClientId(orgId, BussinessCommon.getClientId());
        }
        return org;
    }

    public List<Organization> getOrgByIdList(List<Long> orgId, Boolean active) {
        return orgRepository.getOrgByIdList(BussinessCommon.getClientId(), orgId, active);
    }

    // TODO: remove
    public Long getRootOrgId(Long orgId) {
        if (!orgRepository.existsById(orgId)) {
            return null;
        }
        Long currId = orgId;

        while (true) {
            Long parentId = orgRepository.getParentId(currId, BussinessCommon.getClientId());
            if (parentId == null) {
                break;
            }
            currId = parentId;
        }
        return currId;
    }

    public List<Long> listParentOrg(Long orgId) {
        return listParentOrg(orgId, null);
    }

    public List<Long> listParentOrg(Long orgId, Map<Long, Long> map) {
        if (map == null) {
            map = mapAllParent();
        }
        Set<Long> parentSet = new HashSet<>();
        List<Long> parentList = new ArrayList<>();
        if (!map.containsKey(orgId)) {
            return parentList;
        }
        parentSet.add(orgId);
        parentList.add(orgId);
        Long currId = orgId;

        while (true) {
            if (!map.containsKey(currId)) {
                break;
            }
            Long parentId = map.get(currId);
            if (parentId == null || !parentSet.add(parentId)) {
                break;
            }
            parentList.add(parentId);
            currId = parentId;
        }
        return parentList;
    }

    public List<CategoryInitDto> getListDtoByClientId() {
        List<CategoryInitDto> orgIssuedList = new ArrayList<>();
        List<Organization> orgList = findByClientIdAndActive(BussinessCommon.getClientId(), true);
        if (orgList == null) {
            throw new RestExceptionHandler(Message.NOT_FOUND_ORG);
        }
        orgList.forEach(o -> {
            CategoryInitDto org = new CategoryInitDto();
            org.setId(o.getId());
            org.setName(o.getName());
            orgIssuedList.add(org);
        });
        return orgIssuedList;
    }

    public List<Long> getIdList(Boolean active) {
        return orgRepository.getIdList(BussinessCommon.getClientId(), active);
    }

    public boolean existUserByOrgId(Long orgId) {
        return orgRepository.existUserByOrgId(orgId, BussinessCommon.getClientId());
    }

    public boolean existChildByOrgId(Long orgId) {
        return orgRepository.existChildByOrgId(orgId, BussinessCommon.getClientId());
    }

    public boolean hasInActiveParent(Long orgId) {
        return orgRepository.hasInActiveParent(orgId, BussinessCommon.getClientId());
    }

    public boolean existsByIdAndActive(Long id, boolean active) {
        return orgRepository.existsByIdAndActiveAndClientId(id, active, BussinessCommon.getClientId());
    }

    public List<Long> orgAndSub(Long orgId) {
        Map<Long, Long> map = new WeakHashMap<>();
        List<OrgParent> orgParents = orgRepository.getParentChild(BussinessCommon.getClientId(), "");
        orgParents.forEach(e -> {
            if (e.getChild().equals(e.getParent())) {
                return;
            }
            map.put(e.getChild(), e.getParent());
        });
        return childOfOrg(orgId, map);
    }

    public Calendar2OrgInfo orgInfo(Long orgId) {
        Map<Long, Long> map = new HashMap<>();
        Map<Long, String> mapName = new HashMap<>();
        List<OrgParent> orgParents = orgRepository.getParentChild(BussinessCommon.getClientId(), "");
        orgParents.forEach(e -> {
            if (e.getChild().equals(e.getParent())) {
                return;
            }
            map.put(e.getChild(), e.getParent());
            mapName.put(e.getChild(), e.getName());
        });
        Calendar2OrgInfo info = new Calendar2OrgInfo();
        info.setOrgAndSub(childOfOrg(orgId, map));
        List<Long> parentIds = listParentOrg(orgId, map);
        Long parentId = parentId(orgId, map);
        info.setOrgName(mapName.get(orgId));
        info.setRootOrgName(mapName.get(parentId));
        ConfigSignDto configDto = getConfig(parentIds);
        if (configDto == null) {
            throw new RestExceptionHandler("Không tìm thấy cấu hình ký");
        }
        info.setConfig(getConfig(parentIds));
        return info;
    }

    private ConfigSignDto getConfig(List<Long> parentIds) {
        Map<Long, Integer> idToIndex = new HashMap<>();
        List<ConfigSignDto> configs = orgRepository.findConfigByOrgIds(parentIds, BussinessCommon.getClientId());
        if (configs.isEmpty()) {
            return null;
        }
        for (int i = 0; i < parentIds.size(); ++i) {
            idToIndex.put(parentIds.get(i), i);
        }
        ConfigSignDto lowConfig = configs.get(0);

        for (ConfigSignDto currConfig : configs) {
            if (idToIndex.get(currConfig.getOrgId()) < idToIndex.get(lowConfig.getOrgId())) {
                lowConfig = currConfig;
            }
        }
        return lowConfig;
    }

    private Long parentId(Long child, @NonNull Map<Long, Long> map) {
        Long parentId = child;
        while (true) {
            Long newParentId = map.get(parentId);
            if (newParentId == null) {
                break;
            }
            parentId = newParentId;
        }
        return parentId;
    }

    public Map<Long, Long> mapAllParent() {
        Map<Long, Long> map = new HashMap<>();
        List<OrgParent> orgParents = orgRepository.getParentChild(BussinessCommon.getClientId(), "");
        orgParents.forEach(e -> {
            if (e.getChild().equals(e.getParent())) {
                return;
            }
            map.put(e.getChild(), e.getParent());
        });
        return map;
    }

    private static List<Long> childOfOrg(Long id, Map<Long, Long> mapAllParent) {
        List<Long> all = new ArrayList<>();
        if (id == null) {
            return all;
        }
        all.add(id);
        mapAllParent.forEach((child, parent) -> {
            if (id.equals(parent)) {
                all.addAll(childOfOrg(child, mapAllParent));
            }
        });
        return all;
    }

    public Organization findByNameAndParentId(String name, Long parentId) {
        List<Organization> rsList = orgRepository.findByNameAndParentId(name, parentId, BussinessCommon.getClientId());
        return rsList.isEmpty() ? null : rsList.get(0);
    }

    public List<Organization> getChildrenByOrgTypeAndParentId(Long orgType, Long parentId, Boolean active) {
        List<Organization> data = orgRepository.getChildrenByBpmnIdAndParentId(BussinessCommon.getClientId(), active, parentId, orgType);
        return data;
    }


    public Organization findByName(String name) {
        List<Organization> rsList = orgRepository.getOrgByName(name.toLowerCase(), BussinessCommon.getClientId());
        return rsList.isEmpty() ? null : rsList.get(0);
    }

    public Organization addOrg(Organization org) {
        if (org == null || StringUtils.isNullOrEmpty(org.getIdentifier())
//				|| org.getOrganld() == null
        ) {
            throw new RestExceptionHandler(Message.NO_INPUT_DATA);
        }
        org.set(org);
        validExistName(org.getName(), org.getParentId());
        validExistPhone(org.getPhone(), org.getParentId());
        validExistIdentifier(org.getIdentifier(), null);
        return this.save(org);
    }

    @Override
    @Transactional
    public Organization save(Organization org) {
        Long id = org.getId();
        OrgConfigSign config = OrgConfigSign.valid(org.getOrgConfigSign());
        org.setOrgConfigSign(null);
//        if (config == null) {
//            org = orgRepository.save(org);
//            if (id != null) {
//                orgRepository.deleteOrgSign(id);
//            }
//            return org;
//        }
        if (id == null || id <= 0) {
            return orgRepository.save(org);
        } else {
            OrgConfigSign c = orgRepository.findOrgSign(id);
            if (c != null) {
                c.from(config);
                config = c;
            }
        }
        if (config != null) {
            config.setOrgId(id);
            org.setOrgConfigSign(config);
        }
        return orgRepository.save(org);
    }

    private Organization findByPhoneAndParentId(String phone, Long parentId) {
        List<Organization> rsList = orgRepository.findByPhoneAndParentId(phone, parentId, BussinessCommon.getClientId());
        return rsList.isEmpty() ? null : rsList.get(0);
    }

    private void validExistName(String name, Long parentId) {
        Organization old = findByNameAndParentId(name, parentId);
        if (old != null) {
            throw new RestFieldExceptionHandler("name", Message.ORG_NAME_EXIST);
        }
    }

    private void validExistPhone(String phone, Long parentId) {
        if (!StringUtils.isNullOrEmpty(phone)) {
            Organization olds = findByPhoneAndParentId(phone, parentId);
            if (olds != null) {
                throw new RestFieldExceptionHandler("phone", Message.ORG_PHONE_EXIST);
            }
        }
    }

    private void validExistIdentifier(String nId, String oId) {
        if (!StringUtils.isNullOrEmpty(nId) && !nId.equals(oId)) {
            Organization old = getByIdentifier(nId);
            if (old != null) {
                throw new RestFieldExceptionHandler("identifier", Message.ORG_IDENTIFIER_EXIST);
            }
        }
    }

    public Organization getByIdentifier(String identifier) {
        return orgRepository.findByIdentifierAndClientIdAndActiveTrue(identifier, BussinessCommon.getClientId());
    }

    public Organization updateOrg(Long id, Organization org) {
        Organization old = findByClientIdAndId(id);
        if (old == null || Boolean.TRUE.equals(old.getIsLdap())) {
            throw new RestExceptionHandler(Message.NOT_FOUND_ORG);
        }
        if (org.getParentId() == old.getId()) {
            throw new RestExceptionHandler(Message.ORG_PARENTID_ERROR);
        }
        if (org.getLevel() == 0 && org.getParentId() != null) {
            throw new RestExceptionHandler(Message.ORG_PARENTID_ERROR);
        }

        if (StringUtils.isNullOrEmpty(org.getIdentifier())
//				|| org.getOrganld() == null
        )
            throw new RestExceptionHandler(Message.NO_INPUT_DATA);

        if (!org.getName().equals(old.getName()) || !Objects.equals(org.getParentId(), old.getParentId())) {
            validExistName(org.getName(), org.getParentId());
        }
        if (!org.getPhone().equals(old.getPhone())) {
            validExistPhone(org.getPhone(), org.getParentId());
        }
        validExistIdentifier(org.getIdentifier(), old.getIdentifier());
        old.set(org);
        old.setOrgConfigSign(org.getOrgConfigSign());
        return save(old);
    }

    public Page<Organization> search(OrgDto dto, Integer page) {
        return orgRepository.search(dto.convert(dto), BussinessCommon.getClientId(), BussinessCommon.castToPageable(page));
    }

    public List<Organization> findByParentIdAndActiveAAndClientId() {
        return orgRepository.findByParentIdAndActiveAAndClientId(BussinessCommon.getClientId());
    }

    /**
     * get org parent in level orgType and it's child
     *
     * @param orgId
     * @return
     */
    public List<Long> getOrgAndSubByOrgType(Organization org, String orgType) {
        List<Organization> all = orgRepository.findByClientIdAndActive(BussinessCommon.getClientId(), true);
        List<Long> rs = new ArrayList<>();
        rs.add(org.getId());
        Long parentId = org.getParentId();
        while (true) {
            Organization currentOrg = null;
            for (Organization o : all) {
                if (parentId.equals(o.getId())) {
                    currentOrg = o;
                    break;
                }
            }

            if (currentOrg != null) {
                rs.add(currentOrg.getId());
                parentId = currentOrg.getParentId();
                if (orgType.equals(currentOrg.getOrgTypeModel().getName()) || parentId == null) {
                    break;
                }
            }
        }

        return rs;
    }

    public Long getParentByOrgType(Organization org, String orgType) {
        Long currId = org.getParentId();
        while (true) {
            Organization parent = orgRepository.getParentIdObj(currId, BussinessCommon.getClientId());
            if (parent == null || orgType.equals(parent.getOrgTypeModel().getName())) {
                break;
            }
            currId = parent.getParentId();
        }
        return currId;
    }

    /**
     * Does user belong to BAN/CUCVUVIEN/PHONG
     *
     * @param user
     * @param orgTypeName
     * @return
     */
    public boolean isUserOfOrgType(User user, String orgTypeName) {
        return user.getOrgModel() != null && user.getOrgModel().getOrgTypeModel() != null && orgTypeName.equalsIgnoreCase(user.getOrgModel().getOrgTypeModel().getName());
    }

    public List<CategoryInitDto> getOrgAndSub(List<Long> orgIds) {
        List<Organization> orgList = getOrgByIdList(orgIds, true);
        List<CategoryInitDto> dtoList = new ArrayList<>();
        orgList.forEach(o -> {
            dtoList.add(new CategoryInitDto(o.getName(), o.getId()));
        });
        return dtoList;
    }

    public Organization findParentOrgByTypeAndOrgId(String orgType, Organization org) {
        if (orgType.equalsIgnoreCase(org.getOrgTypeModel().getName())) {
            return org;
        }
        if (org.getParentId() == null) {
            return null;
        }
        Optional<Organization> t = orgRepository.findById(org.getParentId());
        if (!t.isPresent()) {
            return null;
        }
        return findParentOrgByTypeAndOrgId(orgType, t.get());
    }

    public List<Long> findParentAndSubAndSameOrgByCurrOrg(Long orgId, boolean active) {
        return orgRepository.findParentAndSubAndSameOrgByCurrOrg(BussinessCommon.getClientId(), orgId, active);
    }

    public List<OrgParent> orgAndSubWithName(Long parentId) {
        Long clientId = BussinessCommon.getClientId();
        Map<Long, OrgParent> map = new HashMap<>();
        List<OrgParent> orgParents = orgRepository.getParentChild(clientId, "");
        orgParents.forEach(e -> {
            if (e.getChild().equals(e.getParent())) {
                return;
            }
            map.put(e.getChild(), e);
        });
        List<Long> orgIds = new ArrayList<>();
        if (parentId == null) {
            orgIds = orgRepository.findByClientIdAndParentIdAndActive(null, clientId, true);
        } else {
            orgIds.add(parentId);
        }
        List<OrgParent> result = new ArrayList<>();
        for (Long orgId : orgIds) {
            result.addAll(childOfOrgWithName(orgId, map));
        }

        setLeadship(result);

        Collections.sort(result, new Comparator<OrgParent>() {
            @Override
            public int compare(OrgParent o1, OrgParent o2) {
                return o1.getOrder().compareTo(o2.getOrder());
            }
        });

        return result;
    }

    private void setLeadship(List<OrgParent> result) {
        List<Long> orgHasLeaderShips = orgRepository.findOrgHasLeaderShip(BussinessCommon.getClientId());
        result.forEach(i -> {
            Optional<Long> oOpt = orgHasLeaderShips.stream().filter(j -> j.equals(i.getChild())).findAny();
            i.setHasLeaderShip(oOpt.isPresent());
        });
    }

    private List<OrgParent> childOfOrgWithName(Long id, Map<Long, OrgParent> mapAllParent) {
        List<OrgParent> all = new ArrayList<>();
        if (id == null) {
            return all;
        }
        all.add(mapAllParent.get(id));
        mapAllParent.forEach((child, parent) -> {
            if (id.equals(parent.getParent())) {
                all.addAll(childOfOrgWithName(child, mapAllParent));
            }
        });
        return all;
    }

    public List<OrgParent> getOrgTreeById(Long id) {
        List<Long> orgIds = orgAndSub(id);
        return orgRepository.getOrgByListId(orgIds);
    }

    //ticket 1747: "hiển thị các phòng trong tổ chức (cục/vụ/viện)"
    public List<Organization> getAllSiblings() {
        List<Organization> rs = new ArrayList<>();
        Long parentId = BussinessCommon.getUser().getOrgModel().getParentId();

        if (parentId == null) {
            rs.add(BussinessCommon.getUser().getOrgModel());
            return rs;
        }

        rs.addAll(orgRepository.findByClientIdAndParentIdAndActiveTrue(BussinessCommon.getClientId(), parentId));
        return rs;
    }

    public List<OrgGroupDto> searchName(String name) {
        List<OrgGroupDto> listOrg = orgRepository.searchName(name, BussinessCommon.getClientId());

        return listOrg;
    }

    public List<IdName> getAllByGlobal() {
        return orgRepository.getAllByGlobal();
    }

    /**
     * Valid organization by connect outside
     *
     * @param orgIds
     */
    public void validOrgIdsByOutside(List<Long> orgIds) {
        if (BussinessCommon.isEmptyList(orgIds))
            throw new RestExceptionHandler(Message.INVALID_DATA_CONNECT_SYSTEM);
        long count = orgRepository.validOrgIdsByOutside(orgIds, BussinessCommon.getClientId());
        if (count != orgIds.size())
            throw new RestExceptionHandler(Message.INVALID_DATA_CONNECT_SYSTEM);
    }

    public List<OrgGroupDto> getOrgGroupDtoAndSub(List<Long> orgIds) {
        List<Organization> orgList = getOrgByIdList(orgIds, true);
        List<OrgGroupDto> dtoList = new ArrayList<>();
        orgList.forEach(o -> {
            dtoList.add(new OrgGroupDto(o.getName(), o.getId()));
        });
        return dtoList;
    }

    public List<Long> orgAndSubByName(Long orgId, String name) {
        Map<Long, Long> map = new WeakHashMap<>();
        List<OrgParent> orgParents = orgRepository.getParentChild(BussinessCommon.getClientId(), name);
        orgParents.forEach(e -> {
            if (e.getChild().equals(e.getParent())) {
                return;
            }
            map.put(e.getChild(), e.getParent());
        });
        return childOfOrg(orgId, map);
    }

    public List<OrgAndUserDto> getTreeOrg(Long type) {
        Organization orgRoot = new Organization();
        List<OrgAndUserDto> listDto = new ArrayList<OrgAndUserDto>();
        List<Organization> childOrg = new ArrayList<Organization>();
        if (type == 1) { // cùng đơn vị
            if (BussinessCommon.getUser().getPositionModel().getIsLeadership() != null && BussinessCommon.getUser().getPositionModel().getIsLeadership()
                    && BussinessCommon.getUser().getOrgModel().getOrgTypeModel().getName() != null && BussinessCommon.getUser().getOrgModel().getOrgTypeModel().getName().equals(Constant.CUC_VU_VIEN)) {
                orgRoot = findByClientIdAndId(BussinessCommon.getUser().getOrg());
                orgRoot.setParentId(null);
                childOrg.add(orgRoot);
                childOrg = getOrgAndUserByOrganizationId(childOrg);
            } else {
                orgRoot = findByClientIdAndId(BussinessCommon.getUser().getOrg());
                orgRoot.setParentId(null);
                childOrg.add(orgRoot);
            }
        } else { // đơn vị cha và các đơn vị ngang cấp
            orgRoot = findByClientIdAndId(BussinessCommon.getUser().getOrgModel().getParentId());
            orgRoot.setParentId(null);
            childOrg.add(orgRoot);
            childOrg = getOrgAndUserByOrganizationId(childOrg);
        }
        listDto = convertTreeToListDto(listDto, childOrg);
        listDto = convertTreeToListUserDto(listDto, type);
        listDto = convertData(listDto);
        return listDto;
    }

    public List<Organization> getOrgAndUserByOrganizationId(List<Organization> list) {
        for (Organization organization : list) {
            List<Organization> childOrg = orgRepository.getOrganizationByParentId(BussinessCommon.getClientId(),
                    organization.getId());
            organization.setChildren(childOrg);
            if (childOrg.size() > 0) {
                getOrgAndUserByOrganizationId(childOrg);
            }
        }
        return list;
    }

    public List<OrgAndUserDto> convertTreeToListDto(List<OrgAndUserDto> listDto, List<Organization> listOrg) {
        for (int j = 0; j < listOrg.size(); j++) {
            OrgAndUserDto orgDto = new OrgAndUserDto();
            orgDto = convertOrgToDto(listOrg.get(j));
            listDto.add(orgDto);
            if (listOrg.get(j).getChildren() != null) {
                convertTreeToListDto(listDto, listOrg.get(j).getChildren());
            }
        }
        return listDto;
    }

    public OrgAndUserDto convertOrgToDto(Organization organization) {
        OrgAndUserDto andUserDto = new OrgAndUserDto();
        andUserDto.setChild(organization.getId());
        andUserDto.setName(organization.getName());
        andUserDto.setParent(organization.getParentId());
        andUserDto.setType(0);
        return andUserDto;

    }

    public OrgAndUserDto convertUserToDto(User user) {
        OrgAndUserDto andUserDto = new OrgAndUserDto();
        andUserDto.setChild(user.getId());
        andUserDto.setName(user.getFullName());
        andUserDto.setParent(user.getOrg());
        andUserDto.setType(1);
        return andUserDto;
    }

    public List<OrgAndUserDto> convertTreeToListUserDto(List<OrgAndUserDto> listDto, Long type) {
        for (int i = 0; i < listDto.size(); i++) {
            if (listDto.get(i).getType() == 0) {
                List<User> listUserByOrganization = new ArrayList<>();
                if (type == 1) {
                    listUserByOrganization = userRepo.findListUserByOrgAndPositionIdAndActive(listDto.get(i).getChild(), null, true, BussinessCommon.getClientId());
                } else {
                    listUserByOrganization = userRepo.getAllUserbyOrg(BussinessCommon.getClientId(), listDto.get(i).getChild());
                }
                for (User user : listUserByOrganization) {
                    listDto.add(convertUserToDto(user));
                }
            }
        }
        return listDto;
    }

    public List<OrgAndUserDto> convertData(List<OrgAndUserDto> listDto) {
        List<OrgAndUserDto> listData = new ArrayList<OrgAndUserDto>();
        if (listDto.size() > 0) {
            for (OrgAndUserDto dto : listDto
            ) {
                if (dto.getType() == 1L) {
                    listData.add(dto);
                }
            }
            for (OrgAndUserDto dto : listDto
            ) {
                if (dto.getType() != 1L) {
                    listData.add(dto);
                }
            }
        }
        return listData;
    }

    public Organization getOrgCucByOrg(Organization org) {
        if (org.getLevel() == 1 || org.getParentId() == null) {
            return org;
        }
        Organization organization = orgRepository.findByClientIdAndId(BussinessCommon.getClientId(), org.getParentId());
        return getOrgCucByOrg(organization);
    }

    public Organization getOrgByUserCreate(Long userCreateId) {
        return orgRepository.getOrganizationByUserCreate(BussinessCommon.getClientId(), userCreateId);
    }

    public Organization findByIdentifierAndClientIdAndActiveTrue(String code) {
        return orgRepository.findByIdentifierAndActiveTrue(code);
    }

    public Organization findByClientIdAndActiveAndId(Long id, Boolean active) {
        return orgRepository.findByClientIdAndActiveAndId(BussinessCommon.getClientId(), active, id);
    }

    public List<Organization> getAllById(Long id) {
        Organization root = findByClientIdAndActiveAndId(id, true);
        return getAllChildren(root);
    }

    public List<Organization> getAllChildren(Organization orgRoot) {
        List<Organization> result = new ArrayList<>();
        if (orgRoot != null) {
            result.add(orgRoot);
            List<Organization> children = findByClientIdAndParentIdAndActive(orgRoot.getId(), true);
            for (Organization org : children) {
                result.addAll(getAllChildren(org));
            }
        }
        return result;
    }

    private Organization getFirstAncestorOrg(Organization currentOrg) {
        if (currentOrg.getParentOrg() != null) {
            return getFirstAncestorOrg(currentOrg.getParentOrg());
        }
        return currentOrg;
    }


    public List<Organization> getCurrentOrgWholeTree(Organization currentOrg) {
        Organization firstAncestor = getFirstAncestorOrg(currentOrg);
        return getAllChildren(firstAncestor);
    }

    public Organization getById(Long orgId) {
        Optional<Organization> orgById = orgRepository.findById(orgId);
        if (orgById.isPresent()) {
            return orgById.get();
        } else {
            throw new RestExceptionHandler("Cơ quan không tồn tại");
        }
    }

    public Long getUserCount(Long orgId, Boolean active) {
        return orgRepository.countUsers(orgId, BussinessCommon.getClientId(), active);
    }

    public Boolean isOrgLeader(Long userId, Long orgId) {
        User user = userRepo.getOne(userId);
        if (user.getOrg().equals(orgId) && user.isLead()) {
            return true;
        } else {
            List<Long> childrenOrgIds = getAllChildren(user.getOrgModel()).stream().map(Organization::getId).collect(Collectors.toList());
            return childrenOrgIds.contains(orgId) && user.isLead();
        }
    }
    private void addAllParentByParentId(Long parentId, List<Organization> results) {
        Organization orgRoot = findByClientIdAndActiveAndId(parentId, true);
        if (orgRoot != null) {
            results.add(orgRoot);
            if (orgRoot.getParentId() != null) {
                addAllParentByParentId(orgRoot.getParentId(), results);
            }
        }
    }

    private void addAllChildren(List<Organization> results, Long parentId) {
        List<Organization> listChildren = findByClientIdAndParentIdAndActive(parentId, true);
        if (listChildren.size() > 0) {
            results.addAll(listChildren);
            for (Organization child: listChildren) {
                addAllChildren(results, child.getId());
            }
        }
    }

    public List<Organization> getAllByParentId(Long parentId, Boolean active) {
        return orgRepository.getAllByParentId(BussinessCommon.getClientId(), active, parentId);
    }

    public List<Organization> getOrgByName(String text) {
        return orgRepository.getOrgByName(text.toLowerCase(), BussinessCommon.getClientId());
    }

    public List<Organization> getAllByText(String text) {
        List<Organization> results = new ArrayList<>();
        if (!text.isEmpty()) {
            results = getOrgByName(text);
        } else {
            results = findByClientIdAndActive(BussinessCommon.getClientId(), true);
        }
        return results;
    }

    public List<User> getOrgLeaders(Long orgId) {
        return orgRepository.getLeaders(orgId, BussinessCommon.getClientId(), true);
    }
}
