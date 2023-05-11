package com.vz.backend.business.service;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.*;
import java.util.stream.Collectors;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.Organization;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.vz.backend.business.domain.BpmnModel2;
import com.vz.backend.business.domain.BpmnModel2.TYPE_DOCUMENT;
import com.vz.backend.business.domain.Condition;
import com.vz.backend.business.domain.NodeModel2;
import com.vz.backend.business.dto.BpmnActiveWrapper;
import com.vz.backend.business.dto.BpmnSearchDto;
import com.vz.backend.business.dto.NodeDto;
import com.vz.backend.business.dto.UserConditionDto;
import com.vz.backend.business.dto.UserDelegateDto;
import com.vz.backend.business.dto.UserPositionBase;
import com.vz.backend.business.repository.IBpmnRepository2;
import com.vz.backend.business.repository.INodeRepository2;
import com.vz.backend.business.util.NodeType;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.AuthorityEnum;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.exception.RestFieldExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.AuthorityUserService;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.OrganizationService;
import com.vz.backend.util.StringUtils;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class BpmnService2 extends BaseService<BpmnModel2> {
    @Value("${configs.bpmn.one-active-for-org: true}")
    private boolean oneActive;
    @Value("${configs.bpmn.require-lead-under: false}")
    private boolean requireLeadUnder;
    @Value("${configs.bpmn.send-siblings: false}")
    private boolean sendSiblings;

    private static List<String> allowType = Arrays.asList("startEvent", "endEvent", "exclusiveGateway", "task",
            "sequenceFlow");

    @Autowired
    private IBpmnRepository2 bpmnRepository;

    @Autowired
    private INodeRepository2 nodeRepository;

    @Autowired
    private OrganizationService organizationService;

    @Autowired
    private DelegateService delegateService;

    @Autowired
    private AuthorityUserService authorityServive;

    @Override
    public IRepository<BpmnModel2> getRepository() {
        return bpmnRepository;
    }

    @Override
	@Transactional
    public BpmnModel2 save(BpmnModel2 bpmn) {
        if (StringUtils.isNullOrEmpty(bpmn.getContent())) {
            bpmn.setContent(BpmnModel2.CONTENT_DEFAULT);
        }
        Map<String, NodeModel2> identMap = new HashMap<>();
        List<NodeModel2> nodes = rawNode(bpmn, identMap);
        mapNodeWithIdent(nodes, identMap);
        mapNodeWithCondition(nodes, bpmn.fullNodes());
        bpmn.setNodes(nodes);

        return preSave(bpmn, true);
    }

	@Transactional
    public BpmnModel2 update(Long id, BpmnModel2 bpmn) {
        log.error("model.getActive(): {}" + bpmn.getActive());
        Optional<BpmnModel2> bpmnOptional = bpmnRepository.findById(id);
        if (!bpmnOptional.isPresent()) {
            throw new RestExceptionHandler("Không tồn tại BPMN có id: " + id);
        }
        BpmnModel2 oldBpmn = bpmnOptional.get();

        String content = bpmn.getContent();
        boolean isChangedContent = !StringUtils.isNullOrEmpty(bpmn.getContent())
                && !content.equals(oldBpmn.getContent());

        // update field
        if (bpmn.getName() != null) {
            oldBpmn.setName(bpmn.getName());
        }
        if (isChangedContent) {
            oldBpmn.setContent(bpmn.getContent());
        }
        if (bpmn.getOrgId() != null) {
            oldBpmn.setOrgId(bpmn.getOrgId());
        }
        if (bpmn.getTypeDocument() != null) {
            oldBpmn.setTypeDocument(bpmn.getTypeDocument());
        }
        if (bpmn.getActive() != null) {
            oldBpmn.setActive(bpmn.getActive());
        }

        List<NodeModel2> oldNodes = oldBpmn.fullNodes();

        if (isChangedContent) {
            updateNodeWithContent(oldBpmn, oldNodes);
        } else {
            log.error("No change content");
        }

        if (isChangedContent || bpmn.fullNodes() != null) {
            mapNodeWithCondition(oldNodes, bpmn.fullNodes());
        }

        return preSave(oldBpmn, isChangedContent);
    }

    @NonNull
    public NodeModel2 getNodeById(Long nodeId) {
        Optional<NodeModel2> nodeOptional = nodeRepository.findById(nodeId);
        if (nodeOptional.isPresent()) {
            return nodeOptional.get();
        }
        throw new RestExceptionHandler("Không tồn tại node với id: " + nodeId);
    }

    public NodeModel2 getById(Long nodeId) {
        Optional<NodeModel2> nodeOptional = nodeRepository.findById(nodeId);
        if (nodeOptional.isPresent()) {
            return nodeOptional.get();
        }
        return null;
    }

    @NonNull
    public List<Object> getUserByNodeId(Long nodeId, boolean subHandle, String text) {
        // if condition org+pos, only check post in repo, check org in (2) | (1)
        List<UserConditionDto> dto = bpmnRepository.getUserByNodeId(nodeId, subHandle, text, BussinessCommon.getClientId());
        List<UserConditionDto> results = new ArrayList<>();
        User user = BussinessCommon.getUser();
        Category position = user.getPositionModel();
        boolean isBreadth = Boolean.TRUE.equals(position.getIsBreadth()) ||
                Boolean.TRUE.equals(user.isLead());
        boolean isSiblings = Boolean.TRUE.equals(position.getIsSiblings());

        // Incoming documents are not transferable to yourself
        NodeModel2 nodeModel = nodeRepository.findByClientIdAndId(BussinessCommon.getClientId(), nodeId);
        if (nodeModel != null && nodeModel.getBpmn() != null) {
            if (TYPE_DOCUMENT.INCOMING.equals(nodeModel.getBpmn().getTypeDocument())) {
                dto.removeIf(i -> user.getId().equals(i.getId()));
            }
        }

        Map<Long, Long> mapParent = organizationService.mapAllParent();
        dto.forEach(d -> {
            // valid user
            if (!d.invalid()) {
                results.add(d);
                return;
            }
            // match the configuration and same current user | (2)
            // check match condition
            Long receiverOrgId = d.getOrg();
            Long currentOrgId = user.getOrg();
            if (receiverOrgId == null || currentOrgId == null) {
                return;
            }
            if (!isDeepSubOrg(receiverOrgId, d.getOrgId(), mapParent)) {
                return;
            }

            // same org
            // requireLeadUnder: near org
            // !requireLeadUnder: one line
            if (receiverOrgId.equals(currentOrgId)) {
                results.add(d);
                return;
            }
            if (d.isForceSameOrg()) {
                return;
            }

            if (requireLeadUnder && isBreadth && d.isBreadth()
                    && isParentOrChild(receiverOrgId, currentOrgId, mapParent)) {
                results.add(d);
                return;
            }

            if (!requireLeadUnder && (isDeepSubOrg(receiverOrgId, currentOrgId, mapParent)
                    || isDeepSubOrg(currentOrgId, receiverOrgId, mapParent))) {
                results.add(d);
                return;
            }

            if (sendSiblings && isSiblings && d.isSiblings() && isSiblings(currentOrgId, receiverOrgId, mapParent)) {
                results.add(d);
                return;
            }
        });

        setRoleDirection(results);
        Set<Long> ids = results.stream().map(UserConditionDto::getId).collect(Collectors.toSet());
        Map<Long, List<UserDelegateDto>> mapUsers = delegateService.getDelegateByIds(ids);
        results.forEach(userDto -> userDto.setDelegateUsers(mapUsers.get(userDto.getId())));
        results.sort(UserPositionBase::sort);
        return removeDupplicateElements(results);
    }

    private void setRoleDirection(List<UserConditionDto> userConditionDto) {
        if (!BussinessCommon.isEmptyList(userConditionDto)) {
            Set<Long> ids = userConditionDto.stream().map(UserConditionDto::getId).collect(Collectors.toSet());
            Set<Long> directAuthors = authorityServive.checkAuthorByIds(ids, AuthorityEnum.DIRECTION_DOCUMENT);
            userConditionDto.forEach(i -> i.setDirectionAuthority(directAuthors.contains(i.getId())));
        }
    }

    /*
     * Check conditionOrgId userOrgId's ancestor
     */
    private static boolean isDeepSubOrg(Long userOrgId, Long conditionOrgId, @NonNull Map<Long, Long> map) {
        Set<Long> set = new HashSet<>();
        if (conditionOrgId == null) {
            return false;
        }
        while (userOrgId != null) {
            if (!set.add(userOrgId)) {
                return false;
            }
            if (userOrgId.equals(conditionOrgId)) {
                return true;
            }
            if (!map.containsKey(userOrgId)) {
                return false;
            }
            userOrgId = map.get(userOrgId);
        }
        return false;
    }

    /**
     * second is parent of first
     *
     * @param childId
     * @param parentId
     * @param map
     * @return
     */
    private boolean isIntestine(Long childId, Long parentId, @NonNull Map<Long, Long> map) {
        if (parentId == null || childId == null) {
            return false;
        }
        return parentId.equals(map.get(childId));
    }

    private boolean isParentOrChild(Long me, Long you, @NonNull Map<Long, Long> map) {
        return isIntestine(me, you, map) || isIntestine(you, me, map);
    }

    private boolean isSiblings(Long me, Long you, @NonNull Map<Long, Long> map) {
        Long myParent = map.get(me);
        Long yourParent = map.get(you);
        log.error("me: {}, you: {}, -> {} -> {}", me, you, myParent, yourParent);
        if (myParent == null || yourParent == null) {
            return false;
        }
        return myParent.equals(yourParent);
    }

    public NodeModel2 updateNode(Long nodeId, NodeModel2 node) {
        NodeModel2 oldNode = getNodeById(nodeId);
        if (node.getName() != null) {
            updateNodeName(oldNode, node.getName());
        }
        if (node.getConditions() == null) {
            throw new RestExceptionHandler("Cấu hình cho node không được 'null'");
        }
        oldNode.initConditions(node);
        return nodeRepository.save(oldNode);
    }

    @Nullable
    public List<NodeDto> getNextNodes(Long nodeId) {
        BpmnActiveWrapper wrapper = nodeRepository.wrapBpmnByNodeId(nodeId);
        if (wrapper == null) {
            throw new RestExceptionHandler("Không tồn tại node có id là: " + nodeId);
        }
        if (Boolean.FALSE.equals(wrapper.getActive())) {
            throw new RestExceptionHandler("Luồng '" + wrapper.getName() + "' đã bị ngưng sử dụng");
        }
        try {
            return nodeRepository.findAutoNextById(nodeId);
        } catch (Exception e) {
            return new ArrayList<>();
        }
    }

    public Map<Long, List<NodeDto>> getNextNodes(List<Long> nodeId, TYPE_DOCUMENT type) {
        List<NodeDto> nodeDtos = nodeRepository.findAutoNextById(nodeId);
        boolean hasStartNode = nodeId.contains(0L);
        Map<Long, List<NodeDto>> rs = new HashMap<>();
        List<NodeDto> startNodes = new ArrayList<>();
        if (hasStartNode) {
            startNodes = nextStartNodeOfBpmn(type, false);
            rs.put(0L, startNodes);
        }
        for (NodeDto i : nodeDtos) {
            Long key = i.getId();
            List<NodeDto> value = rs.get(key);

            if (!rs.containsKey(key)) {
                value = new ArrayList<>();
            }
            value.add(i);
            rs.put(key, value);
        }
        return rs;
    }

    public List<NodeDto> nextStartNodeOfBpmn(TYPE_DOCUMENT typeDocument, Boolean single) {
        User user = BussinessCommon.getUser();
        List<Long> orgIds;
        if (Boolean.TRUE.equals(single)
                && Constant.PHONG.equalsIgnoreCase(user.getOrgModel().getOrgTypeModel().getName())) {
            Long orgId = organizationService.getParentByOrgType(user.getOrgModel(), Constant.CUC_VU_VIEN);
            orgIds = Arrays.asList(user.getOrg(), orgId);
        } else {
            orgIds = organizationService.listParentOrg(user.getOrg());
            if (orgIds.isEmpty()) {
                throw new RestExceptionHandler("Tổ chức" + user.getOrg() + " không tồn tại");
            }
        }
        // OUTCOMING get all
        if (typeDocument == TYPE_DOCUMENT.OUTGOING || typeDocument == TYPE_DOCUMENT.ASSIGN) {
            List<NodeDto> tmp = nodeRepository.nextStartNodeOfBpmn(user.getId(), user.isLead(), user.getPosition(),
                    user.getOrg(), user.getClientId(), orgIds, typeDocument);
            if (!tmp.isEmpty()) {
                return tmp;
            }
        }
        return nodeRepository.simpleNextStartNodeOfBpmn(user.getClientId(), orgIds, typeDocument);
    }

    private void updateNodeWithContent(BpmnModel2 oldBpmn, List<NodeModel2> oldNodes) {
        Set<String> oldIdentSet = new HashSet<>();

        Map<String, NodeModel2> identMap = new HashMap<>();
        List<NodeModel2> newNodes = rawNode(oldBpmn, identMap);

        Iterator<NodeModel2> iterator = oldNodes.iterator();
        while (iterator.hasNext()) {
            NodeModel2 node = iterator.next();

            String ident = node.getIdent();
            if (identMap.containsKey(node.getIdent())) {
                node.from(identMap.get(ident));
                identMap.replace(ident, node);
                oldIdentSet.add(ident);
            } else {
                node.setPrev(null);
                node.setNext(null);
                iterator.remove();
            }
        }

        for (NodeModel2 newNode : newNodes) {
            String ident = newNode.getIdent();
            if (!oldIdentSet.contains(ident)) {
                oldNodes.add(newNode);
            }
        }

        mapNodeWithIdent(oldNodes, identMap);
    }

    private void updateNodeName(NodeModel2 node, String name) {
        BpmnModel2 bpmn = node.getBpmn();
        node.setName(name);
        Document document = parseDocument(bpmn.getContent());
        if (document == null || node.getIdent() == null) {
            return;
        }
        document.getElementsByTagName(NodeType.TASK);
        Element nodeElement = getTaskByIdent(document, node.getIdent());
        if (nodeElement == null) {
            return;
        }
        nodeElement.setAttribute("name", node.getName());
        String content = getStringDocument(document);
        bpmn.setContent(content);
        bpmnRepository.save(bpmn);
    }

    private Element getTaskByIdent(@NonNull Document document, @NonNull String id) {
        NodeList nodes = document.getElementsByTagName(NodeType.TASK);
        for (int i = 0; i < nodes.getLength(); ++i) {
            Node node = nodes.item(i);
            if (node instanceof Element) {
                Element element = (Element) node;
                if (id.equals(element.getAttribute("id"))) {
                    return element;
                }
            }
        }
        log.warn("Can't found element with id {}", id);
        return null;
    }

    private BpmnModel2 preSave(BpmnModel2 bpmn, boolean isChangedContent) {
        duplicateName(bpmn.getName(), bpmn.getId());
        // set bpmn for that org
//		Long parentId = organizationService.getRootOrgIdHasBpmn(bpmn.getOrgId());
//		if (parentId == null) {
//			throw new RestExceptionHandler("Can't found parent org for id: " + bpmn.getOrgId());
//		}
//		bpmn.setOrgId(parentId);
        if (isChangedContent) {
            // start event
            NodeModel2 endNode = null;

            int startNodeCounter = 0;
            for (NodeModel2 node : bpmn.fullNodes()) {
                if (NodeType.START_EVENT.equals(node.getType())) {
                    ++startNodeCounter;
                } else if (NodeType.END_EVENT.equals(node.getType())) {
                    endNode = node;
                }
            }
            if (endNode == null) {
                throw new RestExceptionHandler("Luồng cần có node cuối");
            }
            if (startNodeCounter == 0) {
                throw new RestExceptionHandler("Luồng cần có node bắt đầu");
            }
            if (startNodeCounter > 1) {
                throw new RestExceptionHandler(
                        "Luồng chỉ có 1 node bắt đầu(hiện tại có " + startNodeCounter + "node bắt đầu");
            }
        }

        // update active
        if (oneActive && Boolean.TRUE.equals(bpmn.getActive())) {
            bpmnRepository.updateOneActiveForBpmnId(bpmn.getClientId(), bpmn.getTypeDocument(), bpmn.getOrgId(),
                    bpmn.getId());
        }
        return bpmnRepository.save(bpmn);
    }

    @NonNull
    private List<NodeModel2> rawNode(@NonNull BpmnModel2 bpmn, Map<String, NodeModel2> identMap) {
        // "(<\\/?)\\w+?:" is regex to remove name space
        Document document = parseDocument(bpmn.getContent().replaceAll("(<\\/?)\\w+?:", "$1"));
        if (document == null) {
            throw new RestExceptionHandler("Nội dung XML lỗi");
        }
        NodeList processList = document.getElementsByTagName(BpmnModel2.PROCESS_TAG);
        if (processList.getLength() != 1) {
            throw new RestExceptionHandler(
                    "Luồng chỉ được có 1 tag 'process'(hiện tại có " + processList.getLength() + " tab 'process'");
        }

        Node processNode = processList.item(0);

        if (!(processNode instanceof Element)) {
            throw new RestExceptionHandler("Tag 'process' phải là Element");
        }

        Element processElement = (Element) processNode;

        List<NodeModel2> nodes = new ArrayList<>();

        NodeList nodeChildrent = processElement.getChildNodes();
        for (int i = 0; i < nodeChildrent.getLength(); ++i) {
            Node nodeChild = nodeChildrent.item(i);
            if (!(nodeChild instanceof Element)) {
                log.info("This node isn't Element");
                continue;
            }
            String nodeName = nodeChild.getNodeName();
            if (!allowType.contains(nodeName)) {
                log.info("We don't allow node with type {}", nodeName);
                continue;
            }
            Element element = (Element) nodeChild;
            NodeModel2 node = new NodeModel2();
            node.init(element);
            String ident = node.getIdent();
            node.setBpmn(bpmn);
            nodes.add(node);
            identMap.put(ident, node); // map ident to id
        }

        return nodes;
    }

    private void mapNodeWithIdent(List<NodeModel2> nodes, Map<String, NodeModel2> identMap) {

        for (NodeModel2 node : nodes) {
            String nextStr = node.getNextStr();
            if (!StringUtils.isNullOrEmpty(nextStr) && identMap.containsKey(nextStr)) {
                node.setNext(identMap.get(nextStr));
            }
            String prevStr = node.getPrevStr();
            if (!StringUtils.isNullOrEmpty(prevStr) && identMap.containsKey(prevStr)) {
                node.setPrev(identMap.get(prevStr));
            }
        }
    }

    private void mapNodeWithCondition(List<NodeModel2> nodes, List<NodeModel2> nodeConditions) {
        // nodes with condition from FE, assign to real nodes
        if (nodeConditions == null) {
            throw new RestExceptionHandler("Cấu hình cho node không được 'null'");
        }

        // map ident -> conditions, assign condition to node
        Map<String, NodeModel2> conditionMap = new HashMap<>();
        for (NodeModel2 node : nodeConditions) {
            if (node != null && !StringUtils.isNullOrEmpty(node.getIdent())) {
                conditionMap.put(node.getIdent(), node);
            }
        }

        for (NodeModel2 node : nodes) {
            String ident = node.getIdent();
            if (conditionMap.containsKey(ident)) {
                node.initConditions(conditionMap.get(ident));
            }
        }
    }

    @Nullable
    private Document parseDocument(String content) {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
        factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");

        try {
            DocumentBuilder builder = factory.newDocumentBuilder();
            return builder.parse(new InputSource(new StringReader(content)));
        } catch (ParserConfigurationException | SAXException | IOException e) {
            return null;
        }
    }

    @NonNull
    private String getStringDocument(Document doc) {
        try {
            DOMSource domSource = new DOMSource(doc);
            StringWriter writer = new StringWriter();
            StreamResult result = new StreamResult(writer);
            TransformerFactory tf = TransformerFactory.newInstance();
            tf.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
            tf.setAttribute(XMLConstants.ACCESS_EXTERNAL_STYLESHEET, "");
            Transformer transformer = tf.newTransformer();
            transformer.transform(domSource, result);
            return writer.toString();
        } catch (TransformerException ex) {
            throw new RestExceptionHandler("Không thể lấy string từ document");
        }
    }

    public Page<BpmnSearchDto> search(String name, TYPE_DOCUMENT type, Pageable pageable) {
        return bpmnRepository.search(name, type, BussinessCommon.getClientId(), pageable);
    }

    public void duplicateName(String name, Long id) {
        if (bpmnRepository.existName(name.toLowerCase(), id, BussinessCommon.getClientId())) {
            throw new RestFieldExceptionHandler("name", "Duplicate bpmn name");
        }
    }

    public Boolean getImportDocBookByNodeId(Long nodeId) {
        return nodeRepository.getImportDocBookByNodeId(nodeId);
    }

    public Boolean getReviewRequiredByNodeId(Long nodeId) {
        return nodeRepository.getReviewRequiredByNodeId(nodeId);
    }

    public Boolean getCloseBranchByNodeId(Long nodeId) {
        return nodeRepository.getCloseBranchByNodeId(nodeId);
    }

    public Map<Long, List<NodeDto>> getDataByNodeId(List<Long> nodeIds) {
        List<NodeDto> nodeDtos = nodeRepository.getDataByNodeId(nodeIds);
        Map<Long, List<NodeDto>> rs = new HashMap<>();
        for (NodeDto i : nodeDtos) {
            Long key = i.getId();
            List<NodeDto> value = rs.get(key);
            if (!rs.containsKey(key)) {
                value = new ArrayList<>();
            }
            value.add(i);
            rs.put(key, value);
        }
        return rs;
    }

    public boolean hasNextNode(List<NodeDto> nodes) {
        return nodes.stream().filter(i -> Boolean.FALSE.equals(i.isLastNode())).count() > 0;
    }

    public boolean lastNode(List<NodeDto> nodes) {
        return nodes.stream().filter(i -> i.isLastNode()).count() > 0;
    }

    public boolean reviewRequired(List<NodeDto> nodes) {
        return nodes.stream().filter(i -> Boolean.TRUE.equals(i.isReviewRequired())).count() > 0;
    }

    public List<NodeDto> nodeList(Long node, TYPE_DOCUMENT type) {
        List<NodeDto> nodes;
        if (node == null || node.equals(0L)) {
            nodes = nextStartNodeOfBpmn(type, false);
        } else {
            nodes = getNextNodes(node);
        }
        return nodes;
    }

    public List<Condition> getConditionByNodeIdAndSecurityTrue(Long nodeId) {
        return nodeRepository.getConditionByNodeIdAndSecurityTrue(nodeId);
    }

    public List<Condition> getConditionByNodeId(Long nodeId) {
        return nodeRepository.getConditionByNodeId(nodeId);
    }

    public Boolean isForceCloseBranchByNodeId(Long nodeId) {
        if (nodeId == null)
            return false;
        return nodeRepository.isForceCloseBranchByNodeId(nodeId);
    }

    public List<Object> removeDupplicateElements(List<UserConditionDto> listData) {
        LinkedHashSet<UserConditionDto> hashSet = new LinkedHashSet<UserConditionDto>(listData);
        return Arrays.asList(hashSet.toArray());
    }

    public List<BpmnModel2> getByClientIdAndListOrg(TYPE_DOCUMENT typeDocument, Boolean single) {
        User user = BussinessCommon.getUser();
        List<Long> orgIds;
        if (Boolean.TRUE.equals(single)
                && Constant.PHONG.equalsIgnoreCase(user.getOrgModel().getOrgTypeModel().getName())) {
            Long orgId = organizationService.getParentByOrgType(user.getOrgModel(), Constant.CUC_VU_VIEN);
            orgIds = Arrays.asList(user.getOrg(), orgId);
        } else {
            orgIds = organizationService.listParentOrg(user.getOrg());
            if (orgIds.isEmpty()) {
                throw new RestExceptionHandler("Tổ chức" + user.getOrg() + " không tồn tại");
            }
        }
        List<BpmnModel2> bpmnModel2s = bpmnRepository.getByClientIdAndListOrg(BussinessCommon.getClientId(), true, orgIds, typeDocument);
        return bpmnModel2s;
    }

    public void checkBpmnIsUsed(Long bpmnId) {
        if (bpmnRepository.checkBpmnIsUsedDocumentIn(bpmnId, BussinessCommon.getClientId())) {
            throw new RestExceptionHandler(Message.BPMN_IS_USED);
        }
        if (bpmnRepository.checkBpmnIsUsedDocumentOut(bpmnId, BussinessCommon.getClientId())) {
            throw new RestExceptionHandler(Message.BPMN_IS_USED);
        }
    }

    public List<Organization> getOrgByNodeId(Long nodeId) {
        List<Organization> results = new ArrayList<>();
        List<Condition> conditions = getConditionByNodeId(nodeId);
        for (Condition condition : conditions) {
            addAllChildrenByOrgIdAndOrgType(results, condition.getOrgId(), condition.getOrgType());
        }
        return results;
    }

    private void addAllChildrenByOrgIdAndOrgType(List<Organization> results, Long orgId, Long orgType) {
        Organization orgParent = organizationService.findByClientIdAndActiveAndId(orgId, true);
        if (orgParent != null && !results.contains(orgParent)) {
            results.add(orgParent);
            List<Organization> childrenList = organizationService.getChildrenByOrgTypeAndParentId(orgType, orgParent.getId(), true);
            for (Organization orgChild : childrenList) {
                if (!results.contains(orgChild)) {
                    addAllChildrenByOrgIdAndOrgType(results, orgChild.getId(), orgType);
                }
            }
        }
    }

    public Long getLastNodeByOrgId(Long orgId, Boolean active) {
        return nodeRepository.getLastNodeByOrgId(BussinessCommon.getClientId(), active, orgId);
    }
}
