package com.vz.backend.business.dto.reportfields;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.vz.backend.business.dto.task.TaskListDto;
import com.vz.backend.core.dto.LabelValueDto;

import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class ReportFieldsTask extends ReportFieldDf {
	private String taskName;
	private Date startDate;
	private Date endDate;
	private String codeTask;
	private String assigner;
	private String handler;
	private String taskStatus;

	@Override
	public Map<String, ? extends Object> getKVMap() {
		Map<String, Object> kvMap = new HashMap<>();
		kvMap.put("taskName", LabelValueDto.set(this.taskName, "Tên công việc"));
		kvMap.put("startDate", LabelValueDto.set(this.startDate, "Ngày bắt đầu"));
		kvMap.put("endDate", LabelValueDto.set(this.endDate, "Ngày kết thúc"));
		kvMap.put("codeTask", LabelValueDto.set(this.codeTask, "Mã công việc"));
		kvMap.put("assigner", LabelValueDto.set(this.assigner, "Người giao"));
		kvMap.put("handler", LabelValueDto.set(this.handler, "Người xử lý"));
		kvMap.put("taskStatus", LabelValueDto.set(this.taskStatus, "Trạng thái"));
		return kvMap;
	}

	public ReportFieldsTask(TaskListDto task) {
		this.taskName = task.getTaskName();
		this.startDate = task.getStartDate();
		this.endDate = task.getEndDate();
		this.codeTask = task.getCodeTask();
		this.assigner = task.getUserAssignName();
		this.handler = task.getUserExcutePrimaryName();
		this.taskStatus = task.getStatusName();
	}

	public ReportFieldsTask(TaskListDto task, int no) {
		this(task);
		super.setNo(no);
	}

	@Override
	public List<ReportFieldDf> cast(List<? extends Object> data) {
		List<ReportFieldDf> tasks = new ArrayList<>();
		int no = 1;
		for (Object i : data) {
			tasks.add(new ReportFieldsTask((TaskListDto) i, no));
			no++;
		}
		return tasks;
	}
}
