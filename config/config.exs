import Config

config :logger,
  level: :debug,
  handle_otp_reports: true,
  handle_sasl_reports: true

config :anoma_lib, []
config :anoma_node, []
config :event_broker, []
