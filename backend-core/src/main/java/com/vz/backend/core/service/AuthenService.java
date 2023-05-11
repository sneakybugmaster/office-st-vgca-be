package com.vz.backend.core.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import com.vz.backend.core.auth.UserLogin;
import com.vz.backend.core.domain.User;
/**
 * @author DucND
 * @date Apr 15, 2020
 */
@Service
public class AuthenService implements UserDetailsService {

	@Autowired
	private UserService userService;
	
	@Override
	public UserDetails loadUserByUsername(String userName) throws UsernameNotFoundException {
		User user = userService.findByUserNameAndActive(userName, true);
		if (user == null) {
			throw new UsernameNotFoundException(userName);
		}
		return new UserLogin(user);
	}
}
