package com.vz.backend.core.util;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.vz.backend.core.domain.User;
import com.vz.backend.core.service.UserService;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

/**
 * @author DucND
 * @date 9 thg 8, 2020
 */
@Slf4j
@Data
@AllArgsConstructor
@NoArgsConstructor
@Component
public class SynchronizeLdap implements Runnable {

	List<User> userList = new ArrayList<>();
	@Autowired
	private UserService userService;

	void SyncUser() {
		if (userList != null && userList.size() > 0) {
			User item;
			User temp;
			List<User> tempList = new ArrayList<>();
			for (int i = 0; i < userList.size(); i++) {
				item = userList.get(i);
				temp = userService.findByUserNameForLdap(item.getUserName(), item.getClientId());
				if (temp == null) {
					temp = item;
				} else {
					temp.setFullName(item.getFullName());
					temp.setActive(item.getActive());
					temp.setEmail(item.getEmail());
					temp.setPhone(item.getPhone());
				}
				tempList.add(temp);
			}
			userService.saveAll(tempList);
			log.info(String.valueOf(tempList.size()));
			log.info("End SyncUser success");
		}
	}

	void SyncOrg() {

	}

	@Override
	public void run() {
		if (userList != null && userList.size() > 0) {
			log.info("Start SyncUser");
			SyncUser();
		}
	}
}
