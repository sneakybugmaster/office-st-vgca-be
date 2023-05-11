package com.vz.backend.core.auth;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * @author DucND
 * @date Apr 15, 2020
 */
@Data
@AllArgsConstructor
public class UserLogin implements UserDetails {

	private User userInfo;

	private static final long serialVersionUID = 1L;

	@Override
	public Collection<? extends GrantedAuthority> getAuthorities() {
		Set<GrantedAuthority> grantedAuthorities = new HashSet<>();
//		for (Role role: userInfo.getRoles()) {
//			grantedAuthorities.add(new SimpleGrantedAuthority(role.getName()));
//		}
		return grantedAuthorities;
	}

	@Override
	public String getPassword() {
		// TODO Auto-generated method stub
		return userInfo.getPassword();
	}

	@Override
	public String getUsername() {
		// TODO Auto-generated method stub
		return userInfo.getUserName();
	}

	@Override
	public boolean isAccountNonExpired() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isAccountNonLocked() {
		return false;
	}

	@Override
	public boolean isCredentialsNonExpired() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isEnabled() {
		// TODO Auto-generated method stub
		return false;
	}
}
