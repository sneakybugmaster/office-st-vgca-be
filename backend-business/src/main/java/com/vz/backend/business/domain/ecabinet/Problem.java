package com.vz.backend.business.domain.ecabinet;

import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.OneToMany;
import javax.persistence.PrePersist;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.config.ecabinet.VoteEnum;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.exception.RestExceptionHandler;

import lombok.Data;

/**
 * Vấn đề xin ý kiến theo từng nội dung họp
 *
 */
@Table(name = "PROBLEM", schema = "vz")
@Entity
@Data
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy", "active", "clientId" })
public class Problem extends BaseModel {

	private static Problem instance;

	public Problem() {
	}

	public static Problem getInstance() {
		if (instance == null) {
			instance = new Problem();
		}
		return instance;
	}

	/**
	 * Nội dung
	 */
	@Column(name = "content", columnDefinition = "TEXT")
	private String content;

	/**
	 * Cần biểu quyết
	 */
	@Column(name = "vote")
	private Boolean isVote;

	/**
	 * Phân loại biểu quyết: Trên hệ thống/ Ngoài hệ thống
	 */
	@Column(name = "type")
	@Enumerated(EnumType.STRING)
	private VoteEnum type;

	/**
	 * Chọn mẫu : Biểu quyết/ 1 Phương án/ n Phương án
	 */
	@Column(name = "[option]")
	@Enumerated(EnumType.STRING)
	private VoteEnum option;

	/**
	 * Số lượng phương án lựa chọn
	 */
	@Column(name = "num_opt")
	private int numOption;

	/**
	 * Danh sách Phương án
	 */
	@OneToMany(mappedBy = "problemId", cascade = { CascadeType.ALL }, orphanRemoval = true)
	private List<Solutions> solutions;

	@Column(name = "agenda_id")
	private Long agendaId;

	/**
	 * bắt đầu biểu quyết
	 */
	@Column(name = "startVote")
	private Boolean startVote;

	/**
	 * Trạng thái biểu quyết
	 */
	@Column(name = "status")
	@Enumerated(EnumType.STRING)
	private VoteEnum status;

	/**
	 * Thời gian biểu quyết
	 * Tính theo phút
	 * Tối đa 1 ngày = 24 x 60 = 1440
	 */
	private Integer voteTimes;

	@Override
	public void valids() {
		BussinessCommon.require("Vấn đề", this.content);
		BussinessCommon.require("Loại biểu quyết", this.type);
		BussinessCommon.require("Chọn mẫu", this.option);
		BussinessCommon.validLengthData(this.content, "Nội dung", 2000);

		if (VoteEnum.CHOOSE_MULTI.equals(this.option)) {
			BussinessCommon.require("Số phương án được chọn", this.numOption);
		}

		if (!BussinessCommon.isEmptyList(this.solutions)) {
			this.solutions.forEach(Solutions::valids);
		}

		if (voteTimes != null && (voteTimes.intValue() < 0 || voteTimes.intValue() > 1440)) {
			throw new RestExceptionHandler("Thời gian biểu quyết không hợp lệ");
		}
	}

	public void setObjId(List<Problem> problems, Long objId) {
		if (BussinessCommon.isEmptyList(problems))
			return;
		problems.forEach(i -> i.setAgendaId(objId));
	}

	public void set(Problem problem) {
		this.content = problem.getContent();
		this.isVote = problem.getIsVote();
		this.type = problem.getType();
		this.option = problem.getOption();
		this.numOption = problem.getNumOption();
		this.agendaId = problem.getAgendaId();
		this.voteTimes = problem.getVoteTimes();
	}

	public Problem(String content, Boolean isVote, VoteEnum type, VoteEnum option, int numOption, Long agendaId) {
		super();
		this.content = content;
		this.isVote = isVote;
		this.type = type;
		this.option = option;
		this.numOption = numOption;
		this.agendaId = agendaId;
	}

	@PrePersist
	public void prePersit() {
		if (this.startVote == null) {
			this.startVote = false;
		}

		if (this.status == null) {
			this.status = VoteEnum.VOTING;
		}
	}
}
