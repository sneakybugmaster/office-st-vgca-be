package com.vz.backend.core.service;

import java.util.ArrayList;
import java.util.List;

import com.vz.backend.core.dto.OrgGroupDto;
import com.vz.backend.core.dto.UserInfoDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.Group;
import com.vz.backend.core.domain.GroupUser;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.GroupWithListUserInfoDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IGroupRepository;
import com.vz.backend.core.repository.IGroupUserRepository;
import com.vz.backend.core.repository.IRepository;
import org.springframework.transaction.annotation.Transactional;

@Service
public class GroupService extends BaseService<Group> {

	@Autowired
	private IGroupRepository groupRepository;
	
	@Autowired
	private IGroupUserRepository groupUserRepository;

	@Override
	public IRepository<Group> getRepository() {
		return groupRepository;
	}

	public Group save(Group gr) {
		try {
			gr = groupRepository.save(gr);
		} catch (Exception e) {
			throw new RestExceptionHandler("Tên nhóm đã tồn tại");
		}
		return gr;
	}
	
	public boolean add(Long groupId, List<Long> listUser) {
		List<GroupUser> listData = new ArrayList<>();
		if (listUser != null && !listUser.isEmpty()) {
			GroupUser item;
			List<GroupUser> userOfGroup = groupUserRepository.findByGroupIdAndClientId(groupId, BussinessCommon.getClientId());
			if (userOfGroup != null && !userOfGroup.isEmpty()) {
				for (GroupUser element : userOfGroup) {
					element.setActive(false);
				}
			}
			for (Long userId : listUser) {
				if (userOfGroup != null && !userOfGroup.isEmpty()) {
					for (int j = 0; j < userOfGroup.size(); j++) {
						if (userOfGroup.get(j).getUserId().equals(userId)) {
							userOfGroup.get(j).setActive(true);
							break;
						}
						// last item
						if (j == userOfGroup.size() - 1) {
							item = new GroupUser();
							item.setGroupId(groupId);
							item.setActive(true);
							item.setUserId(userId);
							listData.add(item);
						}
					}
				} else {
					item = new GroupUser();
					item.setGroupId(groupId);
					item.setActive(true);
					item.setUserId(userId);
					listData.add(item);
				}
			}
			groupUserRepository.saveAll(listData);
		}
		return true;
	}

	public Page<GroupWithListUserInfoDto> getAllGroup(Boolean active, String groupName, String description, Long nodeId, Pageable pageable) {
		User user = BussinessCommon.getUser();
		Page<GroupWithListUserInfoDto> result = groupRepository.getAllGroupByUserId(user.getId(), active, groupName, description, nodeId, user.getClientId(), pageable);
		for (GroupWithListUserInfoDto element : result) {
			element.setListUser(getAllUserInGroup(element.getId(), ""));
		}
		return result;
	}

	public List<UserInfoDto> getAllUserInGroup(Long groupId, String name) {
		return groupUserRepository.findUserInfoByGroupIdAndClientId(groupId, BussinessCommon.getClientId(), name.toLowerCase());
	}

	public Page<UserInfoDto> pageUserInGroup(Long groupId, String name, Pageable pageable) {
		return groupUserRepository.pageUserInGroup(groupId, BussinessCommon.getClientId(), name.toLowerCase(), pageable);
	}

	@Transactional
	public boolean delete(Long groupId) {
		User u = BussinessCommon.getUser();
		groupRepository.deleteByGroupIdAndCreateByAndClientId(groupId, u.getId(), u.getClientId());
		return true;
	}

	public boolean active(Long groupId) {
		User u = BussinessCommon.getUser();
		Group g = groupRepository.findByClientIdAndId(u.getClientId(), groupId);
		if (g == null)
			throw new RestExceptionHandler("Không tìm thấy");
		g.setActive(true);
		groupRepository.save(g);
		return true;
	}
	
	public boolean deactive(Long groupId) {
		User u = BussinessCommon.getUser();
		Group g = groupRepository.findByClientIdAndId(u.getClientId(), groupId);
		if (g == null)
			throw new RestExceptionHandler("Không tìm thấy");
		g.setActive(false);
		groupRepository.save(g);
		return true;
	}
	public List<OrgGroupDto> searchName(String name) {
		List<OrgGroupDto> listGroup=groupRepository.searchName(name, BussinessCommon.getClientId(), BussinessCommon.getUserId());
		return listGroup;
	}
}
