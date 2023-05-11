package com.vz.backend.website;

import com.vz.backend.business.thread.SyncFileVBDen;
import com.vz.backend.core.thread.LdapConnection;
import org.jasig.cas.client.session.SingleSignOutFilter;
import org.jasig.cas.client.validation.Cas30ServiceTicketValidator;
import org.jasig.cas.client.validation.TicketValidator;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.cas.ServiceProperties;
import org.springframework.security.cas.authentication.CasAuthenticationProvider;
import org.springframework.security.cas.web.CasAuthenticationFilter;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.web.authentication.logout.LogoutFilter;
import org.springframework.security.web.authentication.logout.SecurityContextLogoutHandler;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import com.vz.backend.business.service.DocumentBookService;
import com.vz.backend.core.auth.TokenHelper;
import com.vz.backend.core.thread.HandleTrashCan;
import com.vz.backend.core.thread.SynchronizeData;

import springfox.documentation.swagger2.annotations.EnableSwagger2;

@SpringBootApplication
@EntityScan(basePackages = { "com.vz" })
@ComponentScan(basePackages = { "com.vz" })
@EnableJpaRepositories(basePackages = { "com.vz" })
@EnableConfigurationProperties
@EnableJpaAuditing
@EnableWebMvc
@EnableCaching
@EnableSwagger2
@EnableScheduling
@EnableAsync
public class BackendWebsiteApplication implements ApplicationContextAware {

	@Value("${cas.return-login}")
	private String returnLogin;
	@Value("${cas.domain}")
	private String domain;
	@Value("${cas.logout}")
	private String logout;
	@Value("${configs.ldap: false}")
	private boolean ldap;
	@Value("${ftpConfigs.active:false}")
	private boolean ftp;

	private ApplicationContext context;

	@Autowired
	public TokenHelper tokenHelper;
	
	@Autowired
	private DocumentBookService docBookService;

	public static void main(String[] args) {
		SpringApplication.run(BackendWebsiteApplication.class, args);

//		ThreadPoolTaskExecutor taskExecutor = (ThreadPoolTaskExecutor) context.getBean("taskExecutor");
//		SynchronizeData synchronizeData = (SynchronizeData) context.getBean(SynchronizeData.class);
//		taskExecutor.execute(synchronizeData);
	}

	@Bean
	public CasAuthenticationFilter casAuthenticationFilter(AuthenticationManager authenticationManager,
			ServiceProperties serviceProperties) throws Exception {
		CasAuthenticationFilter filter = new CasAuthenticationFilter();
		filter.setAuthenticationManager(authenticationManager);
		filter.setServiceProperties(serviceProperties);
		return filter;
	}

	@Bean
	public ServiceProperties serviceProperties() {
		ServiceProperties serviceProperties = new ServiceProperties();
		serviceProperties.setService(returnLogin);
		serviceProperties.setSendRenew(false);
		return serviceProperties;
	}

	@Bean
	public TicketValidator ticketValidator() {
		return new Cas30ServiceTicketValidator(domain);
	}

	@Bean
	public CasAuthenticationProvider casAuthenticationProvider(TicketValidator ticketValidator,
			ServiceProperties serviceProperties) {
		CasAuthenticationProvider provider = new CasAuthenticationProvider();
		provider.setServiceProperties(serviceProperties);
		provider.setTicketValidator(ticketValidator);
		provider.setUserDetailsService(s -> new User("test@test.com", "Mellon", true, true, true, true,
				AuthorityUtils.createAuthorityList("ROLE_ADMIN")));
		provider.setKey("CAS_PROVIDER_LOCALHOST_8900");
		return provider;
	}

	@Bean
	public SecurityContextLogoutHandler securityContextLogoutHandler() {
		return new SecurityContextLogoutHandler();
	}

	@Bean
	public LogoutFilter logoutFilter() {
		LogoutFilter logoutFilter = new LogoutFilter(logout, securityContextLogoutHandler());
		logoutFilter.setFilterProcessesUrl("/cas/logout");
		return logoutFilter;
	}

	@Bean
	public SingleSignOutFilter singleSignOutFilter() {
		SingleSignOutFilter singleSignOutFilter = new SingleSignOutFilter();
		singleSignOutFilter.setLogoutCallbackPath("/exit/cas");
		singleSignOutFilter.setIgnoreInitConfiguration(true);
		return singleSignOutFilter;
	}

	@Scheduled(cron = "0 0/10 * * * ?")
	public void printNow() {
		if (context != null) {
//			LdapConnection ldapConnection = context.getBean(LdapConnection.class);
//			ldapConnection.connect();

			//sync data
			ThreadPoolTaskExecutor taskExecutor = (ThreadPoolTaskExecutor) context.getBean("taskExecutor");
			LdapConnection synchronizeData = context.getBean(LdapConnection.class);
			taskExecutor.execute(synchronizeData);
		}
	}
	
	@Scheduled(cron = "0 0 0 1 JAN ?")
	public void lockDocumentBook() {
		docBookService.lockDocumentBook();
	}

	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		context = applicationContext;
		printNow();
	}
	
	@Scheduled(cron = "0 0 23 * * ?")
	public void handleTrashCan() {
		if (ftp) {
			ThreadPoolTaskExecutor taskExecutor = (ThreadPoolTaskExecutor) context.getBean("taskExecutor");
			HandleTrashCan handleTrashCan = context.getBean(HandleTrashCan.class);
			taskExecutor.execute(handleTrashCan);
		}
	}

	@Scheduled(fixedDelay = 5000)
	public void SyncVB() {
		if (context != null) {
			//sync data
			SyncFileVBDen syncFileVBDen = context.getBean(SyncFileVBDen.class);
			syncFileVBDen.run();
		}
	}
	
}

	 
