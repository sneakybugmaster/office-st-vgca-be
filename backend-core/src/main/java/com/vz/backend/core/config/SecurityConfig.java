package com.vz.backend.core.config;

import org.jasig.cas.client.session.SingleSignOutFilter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.cas.ServiceProperties;
import org.springframework.security.cas.authentication.CasAuthenticationProvider;
import org.springframework.security.cas.web.CasAuthenticationEntryPoint;
import org.springframework.security.cas.web.CasAuthenticationFilter;
import org.springframework.security.config.BeanIds;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.authentication.logout.LogoutFilter;

/**
 * @author DucND
 * @date Apr 5, 2020
 */
@EnableWebSecurity
public class SecurityConfig extends WebSecurityConfigurerAdapter {

	private SingleSignOutFilter singleSignOutFilter;
	private LogoutFilter logoutFilter;
	private CasAuthenticationProvider casAuthenticationProvider;
	private ServiceProperties serviceProperties;

	@Value("${cas.return-login}")
	private String returnLogin;
	@Value("${cas.domain}")
	private String domain;
	@Value("${cas.logout}")
	private String logout;
	@Value("${cas.login}")
	private String login;

	private static final String[] AUTH_WHITELIST = { "/api/users/login", "/swagger-resources/**",
			"/swagger-ui.html", "/api/users/loginIAM",
			"/error", "/v2/api-docs", "/webjars/**", "/cas/login", "/api/users/login/cas", "/api/users/login/tk", "/logout/cas", "/logout",
			"/cas/validate", "/api/users/version", "/api/files/avatar/**", 
			"/api/saml/**", "/api/users/forget-password", "/api/users/remember-password", "/api/integrate/rs-connect","/api/hstl-form/update/**","/api/hstl-form/download/**",
			"/api/hstl-form/folders/update/**"};

	@Autowired
	public SecurityConfig(SingleSignOutFilter singleSignOutFilter, LogoutFilter logoutFilter,
			CasAuthenticationProvider casAuthenticationProvider, ServiceProperties serviceProperties) {
		this.logoutFilter = logoutFilter;
		this.singleSignOutFilter = singleSignOutFilter;
		this.serviceProperties = serviceProperties;
		this.casAuthenticationProvider = casAuthenticationProvider;
	}

	@Bean
	public AuthenticationProvider jwtAuthenticationFilter() {
		return new AuthenticationProvider();
	}

	@Bean(BeanIds.AUTHENTICATION_MANAGER)
	@Override
	public AuthenticationManager authenticationManagerBean() throws Exception {
		return super.authenticationManagerBean();
	}

	@Bean
	public PasswordEncoder passwordEncoder() {
		return new BCryptPasswordEncoder();
	}

	@Override
	protected void configure(AuthenticationManagerBuilder auth) throws Exception {
		auth.authenticationProvider(casAuthenticationProvider);
	}

	@Override
	public void configure(WebSecurity web) throws Exception {
		web.ignoring().antMatchers(AUTH_WHITELIST);
	}

	@Override
	protected void configure(HttpSecurity http) throws Exception {

		http.cors().and().authorizeRequests().antMatchers(AUTH_WHITELIST).permitAll().anyRequest().authenticated().and()
		.exceptionHandling().authenticationEntryPoint(authenticationEntryPoint()).and()
		.addFilterBefore(singleSignOutFilter, CasAuthenticationFilter.class)
		.addFilterBefore(logoutFilter, LogoutFilter.class).csrf().ignoringAntMatchers("/exit/cas");

		http.csrf().disable();
		// Check token in one per request
		http.addFilterBefore(jwtAuthenticationFilter(), UsernamePasswordAuthenticationFilter.class);
	}

	public AuthenticationEntryPoint authenticationEntryPoint() {
		CasAuthenticationEntryPoint entryPoint = new CasAuthenticationEntryPoint();
		entryPoint.setLoginUrl(login);
		entryPoint.setServiceProperties(serviceProperties);
		return entryPoint;
	}

}
