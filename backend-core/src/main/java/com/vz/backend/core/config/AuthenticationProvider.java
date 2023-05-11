package com.vz.backend.core.config;

import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.util.StringUtils;
import org.springframework.web.filter.OncePerRequestFilter;

import com.vz.backend.core.auth.TokenHelper;
import com.vz.backend.core.auth.UserLogin;
import com.vz.backend.core.service.AuthenService;

import lombok.extern.slf4j.Slf4j;

/**
 * @author DucND
 * @date Apr 14, 2020
 */
@Slf4j
public class AuthenticationProvider extends OncePerRequestFilter {
	@Autowired
	private TokenHelper tokenProvider;

	@Autowired
	private AuthenService customUserDetailsService;

	@Override
	protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
			throws ServletException, IOException {
		try {
			SecurityContextHolder.getContext().setAuthentication(getAuthen(request));
			filterChain.doFilter(request, response);
		} catch (UsernameNotFoundException e) {
			response.sendError(HttpServletResponse.SC_UNAUTHORIZED, Message.NOT_FOUND_USER);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private UsernamePasswordAuthenticationToken getAuthen(HttpServletRequest request) {
		String kongHeader = request.getHeader("kong");
		if (kongHeader != null) {
			log.info("[Kong] : `{}`", request.getRequestURI());
		}
		log.info("Check filter request");
		String jwt = getJwtFromRequest(request);
		if (!StringUtils.hasText(jwt)) {
			return null;
		}
		if (!tokenProvider.validateToken(jwt)) {
			throw new UsernameNotFoundException("Invalid jwt");
		}
		String username = tokenProvider.getUserIdFromJWT(jwt);
		UserLogin userDetails = (UserLogin) customUserDetailsService.loadUserByUsername(username);
		if (userDetails == null || StringUtils.isEmpty(userDetails.getUsername())) {
			throw new UsernameNotFoundException("Can't find user");
		}
		UsernamePasswordAuthenticationToken authentication = new UsernamePasswordAuthenticationToken(userDetails, null,
				userDetails.getAuthorities());
		authentication.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
		return authentication;
	}

	public static String getJwtFromRequest(HttpServletRequest request) {
		String bearerToken = request.getHeader("Authorization");

		if (bearerToken == null || bearerToken.equals("")) {
			String token = request.getParameter("token");
			if (token != null && !token.equals("")) {
				return token;
			}
			token = request.getParameter("Authorization");
			if (token != null && !token.equals("")) {
				return token;
			}
		}

		if (StringUtils.hasText(bearerToken) && bearerToken.startsWith("Bearer ")) {
			return bearerToken.substring(7);
		}

		return null;
	}
}
