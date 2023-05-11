package com.vz.backend.core.domain;

import java.io.Serializable;
import java.util.List;

import javax.persistence.*;

import com.vz.backend.core.config.SystemEnum;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * @author DucND
 * @date Apr 14, 2020
 */
@Entity
@Table(name = "SYS_MODULE", schema = "vz")
@Getter
@Setter
@NoArgsConstructor
public class Module extends BaseModel implements Serializable {
	private static final long serialVersionUID = 1L;

	@Column(name = "[name]")
	private String name;
	@Column(name = "fa_icon")
	private String faIcon;
	@Column(name = "code")
	private String code;
	@Column(name = "description")
	private String description;
	@Column(name = "is_default")
	private Boolean isDefault;
	@Column(name = "order_number")
	private Long orderNumber;
	@Column(name = "router_path")
	private String routerPath;
	@Column(name = "parent_id")
	private Long parentId;
	@Column(name = "component_name")
	private String componentName;
	@Column(name = "is_parent")
	private Boolean isParent;
	@Transient
	private List<Module> subModule;
	@Transient
	private Boolean isChecked;
	@Column(name = "expanded")
	private Boolean expanded;

	/**
	 * user can control show or hide of module/ menu by expect
	 */
	@Column(name = "hide")
	private Boolean hide;
	@PrePersist
	public void prePersist() {
		if (this.expanded == null) {
			this.expanded = false;
		}
	}

	public Boolean getHide() {
		return this.hide == null ? false : this.hide;
	}

	@Enumerated(EnumType.STRING)
	private SystemEnum site;
}
