CREATE TABLE IF NOT EXISTS app_user (
    id BIGSERIAL PRIMARY KEY,
    nick VARCHAR(255) UNIQUE NOT NULL
);

CREATE TABLE IF NOT EXISTS conversation (
    id BIGSERIAL PRIMARY KEY,
    kind VARCHAR(50) NOT NULL DEFAULT 'direct'
);

CREATE TABLE IF NOT EXISTS conversation_participant (
    conversation_id BIGINT REFERENCES conversation(id) ON DELETE CASCADE,
    user_id BIGINT REFERENCES app_user(id) ON DELETE CASCADE,
    PRIMARY KEY (conversation_id, user_id)
);

CREATE TABLE IF NOT EXISTS message (
    id BIGSERIAL PRIMARY KEY,
    conversation_id BIGINT REFERENCES conversation(id) ON DELETE CASCADE,
    sender_id BIGINT REFERENCES app_user(id) ON DELETE SET NULL,
    body TEXT NOT NULL,
    meta JSONB DEFAULT '{}'::JSONB,
    sent_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    client_msg_id UUID NULL,
    UNIQUE(conversation_id, client_msg_id)
);

CREATE TABLE IF NOT EXISTS message_receipt (
    message_id BIGINT REFERENCES message(id) ON DELETE CASCADE,
    user_id BIGINT REFERENCES app_user(id) ON DELETE CASCADE,
    delivered_at TIMESTAMPTZ,
    read_at TIMESTAMPTZ,
    PRIMARY KEY (message_id, user_id)
);

CREATE INDEX IF NOT EXISTS idx_msg_conv_id_id_desc ON message (conversation_id, id DESC);
CREATE INDEX IF NOT EXISTS idx_msg_conv_time ON message (conversation_id, sent_at DESC);
CREATE INDEX IF NOT EXISTS idx_msg_meta_gin ON message USING GIN (meta);