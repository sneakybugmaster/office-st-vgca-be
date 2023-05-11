package com.vz.backend.business.domain;

import java.util.Date;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "ASSIGN_TASK", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class AssignTask extends BaseModel {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	private String name;
	private Date startDate;
	private Date endDate;
	private User assigner;
	private Float progress;
	// private List<User> assignee;
	private String description;
	private User reporter;
	private String priority;
	// private List<AssignTask> relatedTask;
	// private List<Attachment> attachments;
	private String issue;

	@JsonIgnore
	@JoinColumn(name = "id_cat")
	private Long idCat;

}
